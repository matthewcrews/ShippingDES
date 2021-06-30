module ShippingDES.Act

open ShippingDES.Types

module private TrailerArrives =

    let handle (t: Trailer) (st: State) =
        st.Site.Shipping.LoadingBayQueue.Enqueue t
        st.AddMsg Msg.LoadingBayRequested


module private CompleteMove =

    let private casePalletOrder (ord: CasePalletOrder) (st: State) =
        st.Site.Lifts.FreeLift ()
        st.Site.Shipping.RemainingPalletsToLoad.[int ord.TrailerId] <- st.Site.Shipping.RemainingPalletsToLoad.[int ord.TrailerId] - 1<Pallets>
        st.AddMsg (Msg.ResourceFreed ResourceType.Lift)
        st.AddMsg (Msg.PalletDeliveredToTrailer ord.TrailerId)

    let private storesToCaseAssembly (ord: ReplenishOrder) (st: State) =
        st.Site.Lifts.FreeLift ()
        st.Site.CaseAssembly.ReplenishQueue.Enqueue ord
        st.AddMsg (Msg.ResourceFreed ResourceType.Lift)
        st.AddMsg Msg.ReplenishOrderDelivered

    let private storesToShipping (ord: FullPalletOrder) (st: State) =
        st.Site.Lifts.FreeLift ()
        st.Site.Shipping.RemainingPalletsToLoad.[int ord.TrailerId] <- st.Site.Shipping.RemainingPalletsToLoad.[int ord.TrailerId] - 1<Pallets>
        st.AddMsg (Msg.ResourceFreed ResourceType.Lift)
        st.AddMsg (Msg.PalletDeliveredToTrailer ord.TrailerId)

    let handle (mr: MoveRequest) (st: State) =

        st |>
        match mr with
        | MoveRequest.CaseAssemblyToShipping ord -> casePalletOrder ord
        | MoveRequest.StoresToCaseAssembly ord -> storesToCaseAssembly ord
        | MoveRequest.StoresToShipping ord -> storesToShipping ord


module private LoadReplenishOrder =

    let handle (ord: ReplenishOrder) (st: State) =
        st.Site.CaseAssembly.FreePicker ()
        st.Site.CaseAssembly.Inventory.[int ord.PartIdx].AddAvailable (st.Site.Parts.[int ord.PartIdx].CasePerPallet * 1<Cases>)
        st.AddMsg (Msg.InventoryAdded ord.PartIdx)
        st.AddMsg (Msg.ResourceFreed ResourceType.Picker)
        

module private CompleteOrder =

    let handle (ord: CompleteOrder) (st: State) =

        st |>
        match ord with
        | CompleteOrder.CasePalletOrder cpo



let handle (ct: CycleTimes) (st: State, next: Act) =


    match next with
    | Act.TrailerArrives t -> TrailerArrives.handle t
    | Act.CompleteMove cm -> CompleteMove.handle cm
    | Act.LoadReplenishOrder ord -> LoadReplenishOrder.handle ord
    | Act.CompleteOrder ord -> CompleteOrder.handle ord