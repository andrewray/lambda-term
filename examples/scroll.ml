(*
 * scroll.ml
 * ----------
 * Copyright : (c) 2016, Andy Ray <andy.ray@ujamjar.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)
open Lwt
open LTerm_widget
open LTerm_geom

class scrollable_text = object(self)
  inherit label "the_text" as label

  method full_size = { rows = 199; cols = 40; }

  val mutable offset = 0
  method offset = offset
  method set_offset o = offset <- o

  initializer
    self#on_event
      (function
        | LTerm_event.Mouse m ->
          let open LTerm_mouse in
          label#set_text (Printf.sprintf "mouse %ix%i" m.row m.col); 
          self#queue_draw;
          true
        | _ -> false)

  method set_offset_info (scrollbar : vscrollbar) o = 
    let open LTerm_geom in
    self#set_offset o;
    let info = scrollbar#scroll_info in
    let a = scrollbar#allocation in
    label#set_text (Printf.sprintf 
      "offset=%i\n\
       scroll_bar_size = %i\n\
       scroll_size = %i\n\
       scroll_steps = %i\n\
       window_full_size = %i\n\
       scroll_bar_offset = %i\n\
       calc_offset = %i\n\
       alloc=[%i %i %i %i]"
       offset
       info.scroll_bar_size info.scroll_size info.scroll_steps
       info.window_full_size 
       info.scroll_bar_offset info.calc_offset
       a.col1 a.col2 a.row1 a.row2)

end

let main () = 
  let waiter, wakener = wait () in

  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let text = new scrollable_text in
  let scroll = new vscrollbar (text :> scrollable) in
  let hbox = new hbox in
  hbox#add ~expand:true text;
  hbox#add ~expand:false scroll;

  let up = new button "up" in
  up#on_click (fun () -> text#set_offset_info scroll (text#offset+1));
  let down = new button "down" in
  down#on_click (fun () -> text#set_offset_info scroll (text#offset-1));

  let vbox = new vbox in
  vbox#add hbox;
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false up;
  vbox#add ~expand:false down;
  vbox#add ~expand:false exit;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize (fun () -> run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)
(*
  Lazy.force LTerm.stdout >>= fun term ->
  run term vbox waiter
*)

let () = Lwt_main.run (main ())

