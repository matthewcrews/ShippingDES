module ShippingDES.Types

open System
open System.Collections.Generic
open ShippingDES.Collections

[<Measure>] type Pallets
[<Measure>] type Cases
[<Measure>] type PartIdx
[<Measure>] type TrailerIdx
[<Measure>] type Lift
[<Measure>] type ReachLift
[<Measure>] type Picker
[<Measure>] type OrderId
[<Measure>] type PartCount
[<Measure>] type LoadingBay

type PartNumber = PartNumber of string
type TrailerId = TrailerId of string

[<RequireQualifiedAccess>]
type ResourceType =
    | ReachLift
    | Lift
    | Picker
    | LoadingBay

type Part = {
    PartNumber : PartNumber
    CasePerPallet : int
}

type FullPallet = {
    PartNumber : PartNumber
}

type CasePallet = {
    Cases : IReadOnlyDictionary<Part, int<Cases>>
}

type Trailer = {
    TrailerId : TrailerId
    ArrivalDate : DateTime
    FullPallets : FullPallet[]
    CasePallets : CasePallet[]
}

type FullPalletOrder = {
    OrderId : int<OrderId>
    TrailerId : int<TrailerIdx>
    PartIdx : int<PartIdx>
}

type ReplenishOrder = {
    OrderId : int<OrderId>
    PartIdx : int<PartIdx>
}

type StoresOrder = 
    | FullPalletOrder of FullPalletOrder
    | ReplenishOrder of ReplenishOrder

type CasePalletOrder = {
    OrderId : int<OrderId>
    TrailerId : int<TrailerIdx>
    Cases : (int<PartIdx> * int<Cases>)[]
}

type InventoryRecord<[<Measure>] 'Units> () =
    let mutable available : int<'Units> = 0<_>
    let mutable onOrder : int<'Units> = 0<_>
    let mutable short : int<'Units> = 0<_>

    member val Available = available
    member _.AddAvailable x =
        available <- available + x
    member _.DeductAvailable x =
        available <- available - x

    member val OnOrder = onOrder
    member _.AddOnOrder x =
        onOrder <- onOrder + x
    member _.DeductOnOrder x =
        onOrder <- onOrder - x

    member val Short = short
    member _.AddShort x =
        short <- short + x
    member _.DeductShort x =
        short <- short - x


[<RequireQualifiedAccess>]
type MoveRequest =
    | StoresToCaseAssembly of ReplenishOrder
    | StoresToShipping of FullPalletOrder
    | CaseAssemblyToShipping of CasePalletOrder

[<RequireQualifiedAccess>]
type OrderPlaced =
    | FullPalletOrder of FullPalletOrder
    | CasePalletOrder of CasePalletOrder
    | ReplenishOrder of ReplenishOrder

[<RequireQualifiedAccess>]
type CompleteOrder =
    | FullPalletOrder of FullPalletOrder
    | CasePalletOrder of CasePalletOrder
    | ReplenishOrder of ReplenishOrder

[<RequireQualifiedAccess>]
type Activity =
    | PickCaseForAssembly
    | PickPalletFromStores
    | LoadPalletIntoAssembly
    | MoveStoresToCaseAssembly
    | MoveStoresToShipping
    | MoveCaseAssemblyToShipping

type CycleTimes = Dictionary<Activity, TimeSpan>

type OpenAllocation<'Order, [<Measure>] 'Units> (order: 'Order, remaining: int<'Units>) =
    let mutable remaining = remaining
    
    member val Order = order
    member _.Remaining = remaining

    member _.Deduct x =
        remaining <- remaining - x
    

type Stores (reachLiftCount: int) =
    let mutable reachLifts = reachLiftCount * 1<Lift>

    member val ProcessQueue = Queue<StoresOrder>()

    member _.FreeReachLifts = reachLifts
    member _.AllocReachLift () = reachLifts <- reachLifts - 1<_>
    member _.FreeReachLift () = reachLifts <- reachLifts + 1<_>


type CaseAssembly (partCount: int, pickerCount: int) =
    let mutable freePickers = pickerCount * 1<Picker>

    let inventory : InventoryRecord<Cases>[] =
        Array.zeroCreate partCount

    let allocationQueues : Queue<OpenAllocation<CasePalletOrder, Cases>>[] =
        let aq = Array.zeroCreate partCount
        for idx = 0 to aq.Length - 1 do
            aq.[idx] <- Queue<OpenAllocation<CasePalletOrder, Cases>>()
        aq

    member val ReplenishQueue = Queue<ReplenishOrder>()
    member val AssemblyQueue = Queue<CasePalletOrder>()
    member val Inventory = inventory
    member val AllocationQueues = allocationQueues
    member val RemainingPartsToAllocate = Dictionary<int<OrderId>, int<PartCount>>()

    member _.FreePickers = freePickers
    member _.AllocPicker () = freePickers <- freePickers - 1<Picker>
    member _.FreePicker () = freePickers <- freePickers + 1<Picker>

type Shipping (trailerCount: int, loadingBayCount: int) =
    let mutable freeLoadingBays = loadingBayCount * 1<LoadingBay>

    let hasAssignedBay : bool[] =
        Array.create trailerCount false

    let remainingPalletsToLoad : int<Pallets>[] =
        Array.zeroCreate trailerCount

    member val HasAssignedBay = hasAssignedBay
    member val RemainingPalletsToLoad = remainingPalletsToLoad
    member val LoadingBayQueue = Queue<Trailer>()

    member _.FreeLoadingBays = freeLoadingBays
    member _.AllocLoadingBay () = freeLoadingBays <- freeLoadingBays - 1<LoadingBay>
    member _.FreeLoadingBay () = freeLoadingBays <- freeLoadingBays + 1<LoadingBay>


type Lifts (liftCount: int) =
    let mutable freeLifts = liftCount * 1<Lift>

    member val MoveQueue = Queue<MoveRequest>()

    member _.FreeLifts = freeLifts
    member _.AllocLift () = freeLifts <- freeLifts - 1<Lift>
    member _.FreeLift () = freeLifts <- freeLifts + 1<Lift>

type SiteConfig = {
    ReachLiftCount : int
    LoadingBayCount : int
    PickerCount : int
    LiftCount : int
}

type Site (parts: Part seq, trailers: Trailer seq, config: SiteConfig) =

    let parts =
        parts
        |> Array.ofSeq

    let partToIdx =
        parts
        |> Array.mapi (fun idx part -> part.PartNumber, idx * 1<PartIdx>)
        |> readOnlyDict

    let trailers =
        trailers
        |> Array.ofSeq

    let trailerToIdx =
        trailers
        |> Array.mapi (fun idx trailer -> trailer.TrailerId, idx * 1<TrailerIdx>)

    let completions : DateTime[] =
        Array.zeroCreate trailers.Length

    member val Parts = parts
    member val PartToIdx = partToIdx
    member val Trailers = trailers
    member val TrailerToIdx = trailerToIdx
    member val Stores = Stores config.ReachLiftCount
    member val CaseAssembly = CaseAssembly (parts.Length, config.PickerCount)
    member val Shipping = Shipping (trailers.Length, config.LoadingBayCount)
    member val Lifts = Lifts config.LiftCount
    member val Completions = completions


[<RequireQualifiedAccess>]
type Act =
    | TrailerArrives of Trailer
    | CompleteMove of MoveRequest
    | LoadReplenishOrder of ReplenishOrder
    | CompleteOrder of CompleteOrder

[<RequireQualifiedAccess>]
type Msg =
    | LoadingBayRequested
    | LoadingBayAssigned of Trailer
    | ReplenishOrderLoaded of ReplenishOrder
    | PalletDeliveredToTrailer of int<TrailerIdx>
    | OrderPlaced of OrderPlaced
    | ResourceFreed of ResourceType
    | ReplenishOrderDelivered
    | InventoryAdded of int<PartIdx>


type State (initialId: int, startTime: DateTime, site: Site) =
    let mutable nextId = initialId
    let mutable now = startTime
    let acts = PriorityQueue<DateTime, Act>()
    let msgs = Queue<Msg>()

    member val Site = site

    member _.Now = now
    member _.NextId () =
        let next = nextId
        nextId <- next + 1
        next

    member _.AddAct (dt, act) =
        acts.Add (dt, act)

    member _.TryNextAct () =
        match acts.TryDequeue () with
        | Some (dt, act) ->
            now <- dt
            Some (dt, act)
        | None -> None

    member _.AddMsg msg =
        msgs.Enqueue msg

    member _.TryNextMsg () =
        if msgs.Count > 0 then
            let next = msgs.Dequeue ()
            Some next
        else
            None
