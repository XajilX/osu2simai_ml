type timing = 
  Red of {
    start : int;
    bpm : float;
    pat : int
  }
  | Green of {
    start : int;
    speed : float
  }

type hit_obj = 
  Tap of {
    pos : int;
    start : int
  }
  | Long of {
    pos : int;
    start : int;
    end_t : int
  }

let pos_ho ho = match ho with
  Tap { pos = p; _ } -> p
  | Long { pos = p; _ } -> p

let start_ho ho = match ho with
  Tap { start = s; _ } -> s
  | Long { start = s; _ } -> s

let start_tm tm = match tm with
  Red { start = s; _ } -> s
  | Green { start = s; _ } -> s

let bpm_tm tm = match tm with
  Red { bpm = b; _ } -> b
  | Green _ -> raise Not_found