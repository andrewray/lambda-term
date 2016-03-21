open LTerm_geom

class t = LTerm_widget_base_impl.t

class type scrollable = object

  method get_size : int
    (* size of window to be scrolled *)
  
  method get_viewable_size : int
    (* size of viewable window *)

  method set_offset : int -> unit
    (* set offset within window *)

end

class scrollbar rc scrollable = object(self)
  inherit t rc as super
end

class hscrollbar scrollable = object(self)
  inherit scrollbar "hscrollbar" scrollable as super

  method size_request = { rows = 1; cols = scrollable#get_viewable_size }

  method draw ctx focused = 
    let { rows; cols } = LTerm_draw.size ctx in

    (* size   - total size of the area in the scrollable window
       view   - size of the viewable area
       offset - offset within viewable area

       scroll_size - size of the scroll bar
    *)
    ()


end

class vscrollbar scrollable = object(self)
  inherit scrollbar "vscrollbar" scrollable as super

  method size_request = { rows = scrollable#get_viewable_size; cols = 1 }

end


