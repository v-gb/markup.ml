(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let ensure_tail_calls ?hook:_hook = ignore

let to_cps thread =
  fun throw k ->
    let thread = thread () in
    match Lwt.state thread with
    | Lwt.Return x -> k x
    | Lwt.Fail e -> throw e
    | Lwt.Sleep -> Lwt.on_any thread
                     (fun a -> Markup.exhaust_trampoline (k a))
                     (fun a -> Markup.exhaust_trampoline (throw a));
                   Done

module Adapter =
struct
  type 'a t = 'a Lwt.t

  let return = Lwt.return

  let of_cps f =
    let thread, wake = Lwt.wait () in
    Markup.exhaust_trampoline
      (f (fun a -> Lwt.wakeup_later_exn wake a; Markup.Done)
         (fun a -> Lwt.wakeup_later wake a; Markup.Done));
    thread

  let to_cps = to_cps
end

include Markup.Asynchronous (Adapter)

let lwt_stream s = (fun () -> Lwt_stream.get s) |> stream

let to_lwt_stream s = (fun () -> next s) |> Lwt_stream.from
