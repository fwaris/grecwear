
namespace GestureRec

open System
open System.Collections.Generic
open System.Linq
open System.Text
open UIEvents
open Extensions

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Util
open Android.Views
open Android.Widget

type Action = Call of string | Play of string | SendText of string*string

type Sel = Sel of string * (unit->Sel list) | Action of string*(unit -> Action)

module Menu =
    let sendText (phoneList:(string->unit->Action)->Sel list) =
        [
            "Call you back later"
            "Busy now"
            "Ok"
            "Yes"
            "No"
            "I am ok"
        ]
        |> List.map (fun x -> Sel (x, fun() -> phoneList (fun y -> fun()->SendText(x,y))))

    let phoneList action = ["A"; "B"; "C"; "D"] |> List.map (fun x ->Action(x,action x))

    let playMusic() = ["M1"; "M2"; "M3"] |> List.map (fun x -> Action(x,fun () -> Play x)) 

    let topMenu() = 
        [
            Sel ("Call", fun () -> phoneList (fun x-> fun()->Call x))
            Sel ("Send text", fun () -> sendText phoneList)
            Sel ("Play", playMusic)
        ]

    let label = function Sel(x,_) | Action (x,_) -> x


type NavListAdapter(ctx:Activity, data:Sel list) =
    inherit BaseAdapter<String>()
    override x.GetItemId(i) = int64 i
    override x.Item with get(i) = Menu.label data.[i]
    override x.Count = data.Length

    override x.GetView(pos,convertView,parent) =
        let s = Menu.label data.[pos]
        let v = ctx.LayoutInflater.Inflate(Resource_Layout.SelView,null)
        let txSel = v.FindViewById<TextView>(Resource_Id.txSel)
        txSel.Text <- s
        v

type SelStack = SSList of PrevItemSelected:int*Sel:Sel | SSAction of Event<unit>

type NavigationFragment () =
    inherit Fragment ()
    let mutable eventSubscription = Unchecked.defaultof<_>

    let mutable selectionStack = SSList (PrevItemSelected=0,Sel=Sel("Main",Menu.topMenu))::[]
    let push ss = selectionStack <- ss::selectionStack

    let pop() = //keeps 1 item always
        let (d,ss) = 
            match selectionStack with
            | [] -> None,[]
            | d::[] -> None,selectionStack
            | d::rest -> Some d,rest
        selectionStack <- ss
        d
    
    let top() = match selectionStack with x::_ -> Some x | _ -> None

    let labels xs = xs |> List.map Menu.label |> List.toArray

    override this.OnCreateView(inflater, container, savedInstance) =
        let view = inflater.Inflate(Resource_Layout.Navigation,container,false)
        view

    override this.OnStart() =
        base.OnStart()
        let grd = this.View.FindViewById<GridView>(Resource_Id.grdNav)
        let txBc = this.View.FindViewById<TextView>(Resource_Id.txBreadCrumb)
//        let adapter = new NavListAdapter(this.Activity,data)


        let setList ss =
            let ls = ss |> labels
            grd.Adapter <- new ArrayAdapter<string>(this.Activity,Android.Resource.Layout.SimpleListItemActivated1, ls)

        let menuTrail() = List.rev [for i in selectionStack do match i with SSList(_,s) -> yield Menu.label s]
          
        let setBreadCrumb() = 
            txBc.Text <-  String.Join(">>",menuTrail()|>List.toArray)

        let selectPos i = 
            grd.SetItemChecked(i,true)
            grd.SmoothScrollToPosition(i)

        let movePos i = 
            let g = grd.CheckedItemPosition
            let g2  =
                match g,i with
                | -1,-1 -> grd.Adapter.Count - 1
                | -1,+1 -> 0
                | x,-1  -> max 0 (x-1)
                | x,+1  -> min (grd.Adapter.Count-1) (x + 1)
                | _,_   -> failwith "should not happen"
            logI (sprintf "g=%d, g2=%d, i=%d" g g2 i)
            selectPos g2
        let left() = movePos -1
        let right() = movePos +1

        let performAction a p = 
            let msg = 
                match a with
                | Call x -> sprintf "Calling %s" x
                | SendText (x,y) -> sprintf "Sending text '%s' to %s" x y
                | Play x -> sprintf "Playing '%s'" x
            AndroidExtensions.showAsync this.Activity "performing..." msg (Some p) (fun _ _ -> ())

        let selectDown() = 
            let pos = grd.CheckedItemPosition
            if pos >= 0 then
                match top() with
                | Some (SSList (_,(Sel(_,f) as tp))) ->
                    let ss = f()
                    let s = List.nth ss pos
                    match s with
                    | Sel (_,f) -> 
                        push (SSList (PrevItemSelected=pos, Sel=s))
                        f() |> setList
                    | Action (_,a) -> 
                        let cancelEvent = new Event<_>()
                        let p = cancelEvent.Publish
                        push (SSAction cancelEvent)
                        performAction (a()) p
                | Some (SSAction _) -> () //already performing action
                | x -> failwithf "shoud not happen %A" x
                setBreadCrumb()

        let backout() = 
            match pop() with 
            | Some (SSAction ev) -> ev.Trigger()
            | _ -> 
                match top() with
                | Some (SSList(i,Sel(_,ss))) ->
                    ss() |> setList
                    selectPos i
                | Some _ -> failwith "should not happen"
                | None -> ()
                setBreadCrumb()

        eventSubscription <- onUiEvent.Subscribe(function 
            | Nav n ->
                match n with
                | Left  -> left()
                | Right -> right()
                | Tap   -> selectDown()
                | Swipe -> backout()
                | _     -> ()
            | _ -> ())

        //inititalize top menu
        match top() with Some (SSList(_,Sel(_,f)))-> f() |> setList; selectPos 0 | _ ->()
        setBreadCrumb()

    override this.OnStop()=
        base.OnStop()
        if eventSubscription <> Unchecked.defaultof<_> then eventSubscription.Dispose()
    

