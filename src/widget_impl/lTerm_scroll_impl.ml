open CamomileLibrary
open LTerm_geom

class t = LTerm_widget_base_impl.t

class type scrollable = object
  inherit t

  method full_size : size
    (* full size of scrollable window *)

  method offset : int
    (* offset within window *)

  method set_offset : int -> unit
    (* set offset within window *)

end

module Projection = struct

  (* project back and forth between offsets within the 
     scrollable window and the scroll bar, reversibly *)

  let create full_size steps = 
    let full_size' = float_of_int full_size in
    let steps' = float_of_int (steps-1) in
    Array.init steps 
      (fun scroll ->
        let scroll' = float_of_int scroll in
        max 0 (min full_size 
          (int_of_float @@ ((full_size' *. scroll' /. steps') +. 0.5))))

  let get_step t offset = 
    if offset < 0 then t.(0), t.(1)-1
    else 
      let len = Array.length t in
      if offset >= len - 1 then t.(len-1), t.(len-1)
      else t.(offset), t.(offset+1)-1

  let search a value = 
    (* ordered ranges *)
    let (=:) x y = let (lo,hi) = get_step a x in lo <= y && y <= hi in
    let (<:) x y = let (lo,hi) = get_step a x in hi < y in
    let (>:) x y = let (lo,hi) = get_step a x in lo > y in
    (* binary search *)
    let rec search low high =
      if high = low then
        if low =: value then low
        else raise Not_found
      else 
        let mid = (low + high) / 2 in
        if mid >: value then search low (mid - 1)
        else if mid <: value then search (mid + 1) high
        else mid
    in
    let len = Array.length a in
    if value < 0 then 0
    else if (len-1) <: value then len-1
    else search 0 (len - 1)

  (* create the search array, remembering previous sizes *)
  let make () = 
    let full_size', steps' = ref 0, ref 0 in
    let proj = ref [||] in
    let memo_create full_size steps = 
      if !full_size' = full_size && !steps' = steps then !proj
      else begin
        proj := create full_size steps;
        full_size' := full_size;
        steps' := steps;
        !proj
      end
    in
    (fun full_size steps -> get_step (memo_create full_size steps)),
    (fun full_size steps -> search (memo_create full_size steps))

end

class vscrollbar (scrollable : scrollable) = 
  let get_size (t : #t) = rows @@ size_of_rect @@ t#allocation in
  let proj_get, proj_search = Projection.make () in

  object(self)
    inherit t "scrollbar"

    method can_focus = true

    method size_request = { rows = get_size scrollable; cols = 1 }

    method scroll_info = 
      let scroll_bar_size = 5 in
      let scroll_size = get_size self in
      let scroll_steps = scroll_size - scroll_bar_size + 1 in
      let window_full_size = rows (scrollable#full_size) in

      let scroll_bar_offset = proj_search window_full_size scroll_steps scrollable#offset in
      let calc_offset = fst @@ proj_get window_full_size scroll_steps scroll_bar_offset in

      {
        LTerm_geom.scroll_bar_size; scroll_size; scroll_steps;
        window_full_size; 
        scroll_bar_offset; calc_offset
      }

    method draw ctx focused = 
      let focus = (self :> t) = focused in
      let { cols; _ } = LTerm_draw.size ctx in

      let info = self#scroll_info in
      let c = if focus then '*' else '|' in

      (*  *)
      for col=0 to cols-1 do
        for row=info.scroll_bar_offset to 
                info.scroll_bar_offset+info.scroll_bar_size-1 do
          LTerm_draw.draw_char ctx row col @@ UChar.of_char c
        done
      done

  end


