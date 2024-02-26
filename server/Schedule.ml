open Dream_html
open HTML
open CalendarLib
open Date
open Bwd
open Bwd.Infix

let add_class node str = node +@ class_ "%s" (node.@["class"] ^ " " ^ str)

let day_button (date : Date.t) =
  button
    [ type_ "button"; class_ "py-1.5 hover:bg-gray-100 focus:z-10" ]
    [
      time
        [
          class_ "mx-auto flex h-7 w-7 items-center justify-center rounded-full";
          datetime "%s" (Printer.Date.to_string date);
        ]
        [ txt "%d" (day_of_month date) ];
    ]

let daily start period f =
  let rec step (current : 'a date) (computed : 'b bwd) (left : int) =
    let next_date = next current `Day in
    match left with
    | 0 -> computed
    | _ -> step next_date (computed <: f current) (left - 1)
  in
  step start Emp (Period.nb_days period) @> []

let first_of_month date =
  let m = month date in
  let y = year date in
  make y (int_of_month m) 1

let last_of_month date =
  let m = month date in
  let y = year date in
  make y (int_of_month m) (days_in_month date)

let padding_start date = (int_of_day @@ day_of_week @@ first_of_month date) - 1
let padding_end date = 7 - (int_of_day @@ day_of_week @@ last_of_month date)

let pack_date date =
  let year = year date in
  let month = int_of_month @@ month date in
  let day = day_of_month date in
  Format.sprintf "%d/%d/%d" year month day

let infos =
  let today = Date.today () in
  let now = Time.now () in
  ol
    [
      class_
        "mt-4 divide-y divide-gray-100 text-sm leading-6 lg:col-span-7 \
         xl:col-span-8";
    ]
    [
      li
        [ class_ "relative flex space-x-6 py-6 xl:static" ]
        [
          txt "\nThe number of days in the month is %d\n"
          @@ Date.days_in_month today;
        ];
      li
        [ class_ "relative flex space-x-6 py-6 xl:static" ]
        [
          txt "\nThe number of days in the previous month is %d\n"
          @@ Date.days_in_month (rem today (Period.month 1));
        ];
      li
        [ class_ "relative flex space-x-6 py-6 xl:static" ]
        [ txt "\nThe current time is %s\n" @@ Printer.Time.to_string now ];
      li
        [ class_ "relative flex space-x-6 py-6 xl:static" ]
        [
          txt "\nThe first day of the month is %s"
            (!Printer.day_name @@ day_of_week @@ first_of_month today);
        ];
      li
        [ class_ "relative flex space-x-6 py-6 xl:static" ]
        [
          txt "\nThe last day of the month is %s"
            (!Printer.day_name @@ day_of_week @@ last_of_month today);
        ];
      li
        [ class_ "relative flex space-x-6 py-6 xl:static" ]
        [
          txt "\nThe padding required in the beginning is %d\n"
            (padding_start today);
        ];
      li
        [ class_ "relative flex space-x-6 py-6 xl:static" ]
        [ txt "\nThe padding required in the end is %d\n" (padding_end today) ];
      (* Printer.Date.print "\nThe current day is %d\n" today *)
    ]

let show_in_month : Date.t -> Dream_html.node =
 fun date ->
  let days_in_current_month = days_in_month date in
  let _days = days_in_current_month in
  let wdays =
    div [ class_ "mt-6 grid grid-cols-7 text-xs leading-6 text-gray-500" ]
    @@ List.map
         (fun s -> div [] [ txt s ])
         [ "M"; "T"; "W"; "T"; "F"; "S"; "S" ]
  in

  let days =
    let ps =
      let pd = Period.day (padding_start date) in
      daily (rem (first_of_month date) pd) pd day_button
      |> List.map (fun node -> add_class node "bg-gray-50")
    in

    let pe =
      let pd = Period.day (padding_end date) in
      daily (add (last_of_month date) (Period.day 1)) pd day_button
      |> List.map (fun node -> add_class node "bg-gray-50")
    in
    let month =
      daily (first_of_month date) (Period.day (days_in_month date)) day_button
    in
    ps @ List.map (fun node -> add_class node "bg-white") month @ pe
  in
  div
    [
      id "calendar";
      class_ "mx-auto max-w-lg px-6 py-8 lg:max-2-4xl xl:max-w-6xl";
    ]
    [
      div []
        [
          h2
            [ class_ "text-base font-semibold leading-6 text-gray-900" ]
            [ txt "Upcoming meetings" ];
          div
            [ class_ "lg:grid lg:grid-cols-12 lg:gap-x-16" ]
            [
              div
                [
                  class_
                    "mt-10 text-center lg:col-start-8 lg:col-end-13 \
                     lg:row-start-1 lg:mt-9 xl:col-start-9";
                ]
                [
                  div
                    [ class_ "flex items-center text-gray-900" ]
                    [
                      button
                        [
                          Hx.get "/calendar/%s" (pack_date (prev date `Month));
                          Hx.target "#calendar";
                          Hx.swap "outerHTML";
                          (* Hx.push_url "true"; *)
                          type_ "button";
                          class_
                            "-m-1.5 flex flex-none items-center justify-center \
                             p-1.5 text-gray-400 hover:text-gray-500";
                        ]
                        [
                          span [ class_ "sr-only" ] [ txt "Previous month" ];
                          SVG.svg
                            [
                              class_ "h-5 w-5";
                              SVG.viewbox ~min_x:0 ~min_y:0 ~width:20 ~height:20;
                              SVG.fill "currentColor";
                              (* Aria.hidden true; *)
                            ]
                            [
                              SVG.path
                                [
                                  (* SVG.fill_rule "evenodd"; *)
                                  SVG.d
                                    "M12.79 5.23a.75.75 0 01-.02 1.06L8.832 \
                                     10l3.938 3.71a.75.75 0 11-1.04 \
                                     1.08l-4.5-4.25a.75.75 0 \
                                     010-1.08l4.5-4.25a.75.75 0 011.06.02z";
                                  (* clip _ rule "evenodd"; *)
                                ]
                                [];
                            ];
                        ];
                      div
                        [ class_ "flex-auto text-sm font-semibold" ]
                        [ txt "%s" (Printer.name_of_month @@ month date) ];
                      button
                        [
                          Hx.get "/calendar/%s" (pack_date (next date `Month));
                          Hx.target "#calendar";
                          Hx.swap "outerHTML";
                          (* Hx.push_url "true"; *)
                          type_ "button";
                          class_
                            "-m-1.5 flex flex-none items-center justify-center \
                             p-1.5 text-gray-400 hover:text-gray-500";
                        ]
                        [
                          span [ class_ "sr-only" ] [ txt "Next month" ];
                          SVG.svg
                            [
                              class_ "h-5 w-5";
                              SVG.viewbox ~min_x:0 ~min_y:0 ~width:20 ~height:20;
                              SVG.fill "currentColor";
                              (* Aria.hidden true; *)
                            ]
                            [
                              SVG.path
                                [
                                  (* SVG.fill_rule "evenodd"; *)
                                  SVG.d
                                    "M7.21 14.77a.75.75 0 01.02-1.06L11.168 10 \
                                     7.23 6.29a.75.75 0 111.04-1.08l4.5 \
                                     4.25a.75.75 0 010 1.08l-4.5 4.25a.75.75 0 \
                                     01-1.06-.02z"
                                  (* clip _ rule "evenodd"; *);
                                ]
                                [];
                            ];
                        ];
                    ];
                  wdays;
                  div
                    [
                      class_
                        "isolate mt-2 grid grid-cols-7 gap-px rounded-lg \
                         bg-gray-200 text-sm shadow ring-1 ring-gray-200";
                    ]
                    days;
                ];
              infos;
            ];
        ];
    ]
