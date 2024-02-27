open Dream_html
open HTML

type layout = [ `Mobile | `Desktop ]

let nav_items = [ ("Forest", "/forest"); ("Calendar", "/calendar") ]

let navbar layout =
  let style =
    match layout with
    | `Mobile ->
        class_
          "block rounded-lg py-2 pl-6 pr-3 txt-sm font-semibold leading-7 \
           txt-gray-900 hover:bg-gray-50"
    | `Desktop -> class_ "txt-sm font-semibold leading-6 txt-gray-900"
  in
  List.map
    (fun (t, url) ->
      a
        [
          href "%s" url;
          Hx.get "%s" url;
          Hx.target "#content";
          Hx.swap "innerHTML";
          style;
        ]
        [ txt "%s" t ])
    nav_items

let header =
  header
    [ class_ "bg-white" ]
    [
      nav
        [
          class_
            "mx-auto flex max-w-7xl items-center justify-between p-6 lg:px-8";
          Aria.label "Global";
        ]
        [
          div
            [ class_ "flex lg:flex-1" ]
            [
              a
                [ href "#"; class_ "-m-1.5 p-1.5" ]
                [
                  span [ class_ "sr-only" ] [ txt "Your Company" ];
                  img
                    [
                      class_ "h-8 w-auto";
                      src
                        "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600";
                      alt "";
                    ];
                ];
            ];
          div
            [ class_ "flex lg:hidden" ]
            [
              button
                [
                  type_ "button";
                  class_
                    "-m-2.5 inline-flex items-center justify-center rounded-md \
                     p-2.5 txt-gray-700";
                ]
                [
                  span [ class_ "sr-only" ] [ txt "Open main menu" ];
                  SVG.svg
                    [
                      class_ "h-6 w-6";
                      SVG.fill "none";
                      SVG.viewbox ~min_x:0 ~min_y:0 ~width:24 ~height:24;
                      SVG.stroke_width "1.5";
                      SVG.stroke "currentColor";
                      Aria.hidden true;
                    ]
                    [
                      SVG.path
                        [
                          SVG.stroke_linecap `round;
                          SVG.stroke_linejoin `round;
                          SVG.d "M3.75 6.75h16.5M3.75 12h16.5m-16.5 5.25h16.5";
                        ]
                        [];
                    ];
                ];
            ];
          div
            [ class_ "hidden lg:flex lg:gap-x-12"; Hx.boost true ]
            (navbar `Desktop);
          div
            [ class_ "hidden lg:flex lg:flex-1 lg:justify-end" ]
            [
              a
                [
                  href "/login";
                  class_ "txt-sm font-semibold leading-6 txt-gray-900";
                ]
                [ txt "Log in"; span [ Aria.hidden true ] [ txt "→" ] ];
            ];
        ];
    ]

let page content =
  div
    [ class_ "min-h-full" ]
    [ header; div [ id "content"; class_ "py-10" ] [ content ] ]
