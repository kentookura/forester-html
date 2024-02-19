open Dream_html
open HTML

let header_link c =
  a [ href "#"; class_ "txt-sm font-semibold leading-6 txt-gray-900" ] [ txt c ]

let flyout =
  (*
          'Product' flyout menu; show/hide based on flyout menu state.

          Entering: "transition ease-out duration-200"
            From: "opacity-0 translate-y-1"
            To: "opacity-100 translate-y-0"
          Leaving: "transition ease-in duration-150"
            From: "opacity-100 translate-y-0"
            To: "opacity-0 translate-y-1"
        *)
  div
    [
      class_
        "absolute -left-8 top-full z-10 mt-3 w-screen max-w-md overflow-hidden \
         rounded-3xl bg-white shadow-lg ring-1 ring-gray-900/5";
    ]
    [
      div
        [ class_ "p-4" ]
        [
          div
            [
              class_
                "group relative flex items-center gap-x-6 rounded-lg p-4 \
                 txt-sm leading-6 hover:bg-gray-50";
            ]
            [
              div
                [
                  class_
                    "flex h-11 w-11 flex-none items-center justify-center \
                     rounded-lg bg-gray-50 group-hover:bg-white";
                ]
                [
                  SVG.svg
                    [
                      class_ "h-6 w-6 txt-gray-600 group-hover:txt-indigo-600";
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
                          SVG.d "M10.5 6a7.5 7.5 0 107.5 7.5h-7.5V6z";
                        ]
                        [];
                      SVG.path
                        [
                          SVG.stroke_linecap `round;
                          SVG.stroke_linejoin `round;
                          SVG.d "M13.5 10.5H21A7.5 7.5 0 0013.5 3v7.5z";
                        ]
                        [];
                    ];
                ];
              div
                [ class_ "flex-auto" ]
                [
                  a
                    [ href "#"; class_ "block font-semibold txt-gray-900" ]
                    [ txt "Analytics"; span [ class_ "absolute inset-0" ] [] ];
                  p
                    [ class_ "mt-1 txt-gray-600" ]
                    [ txt "Get a better understanding of your traffic" ];
                ];
            ];
          div
            [
              class_
                "group relative flex items-center gap-x-6 rounded-lg p-4 \
                 txt-sm leading-6 hover:bg-gray-50";
            ]
            [
              div
                [
                  class_
                    "flex h-11 w-11 flex-none items-center justify-center \
                     rounded-lg bg-gray-50 group-hover:bg-white";
                ]
                [
                  SVG.svg
                    [
                      class_ "h-6 w-6 txt-gray-600 group-hover:txt-indigo-600";
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
                          SVG.d
                            "M15.042 21.672L13.684 16.6m0 0l-2.51 \
                             2.225.569-9.47 5.227 7.917-3.286-.672zM12 \
                             2.25V4.5m5.834.166l-1.591 1.591M20.25 \
                             10.5H18M7.757 14.743l-1.59 1.59M6 \
                             10.5H3.75m4.007-4.243l-1.59-1.59";
                        ]
                        [];
                    ];
                ];
              div
                [ class_ "flex-auto" ]
                [
                  a
                    [ href "#"; class_ "block font-semibold txt-gray-900" ]
                    [ txt "Engagement"; span [ class_ "absolute inset-0" ] [] ];
                  p
                    [ class_ "mt-1 txt-gray-600" ]
                    [ txt "Speak directly to your customers" ];
                ];
            ];
          div
            [
              class_
                "group relative flex items-center gap-x-6 rounded-lg p-4 \
                 txt-sm leading-6 hover:bg-gray-50";
            ]
            [
              div
                [
                  class_
                    "flex h-11 w-11 flex-none items-center justify-center \
                     rounded-lg bg-gray-50 group-hover:bg-white";
                ]
                [
                  SVG.svg
                    [
                      class_ "h-6 w-6 txt-gray-600 group-hover:txt-indigo-600";
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
                          SVG.d
                            "M7.864 4.243A7.5 7.5 0 0119.5 10.5c0 2.92-.556 \
                             5.709-1.568 8.268M5.742 6.364A7.465 7.465 0 004.5 \
                             10.5a7.464 7.464 0 01-1.15 3.993m1.989 \
                             3.559A11.209 11.209 0 008.25 10.5a3.75 3.75 0 \
                             117.5 0c0 .527-.021 1.049-.064 1.565M12 \
                             10.5a14.94 14.94 0 01-3.6 9.75m6.633-4.596a18.666 \
                             18.666 0 01-2.485 5.33";
                        ]
                        [];
                    ];
                ];
              div
                [ class_ "flex-auto" ]
                [
                  a
                    [ href "#"; class_ "block font-semibold txt-gray-900" ]
                    [ txt "Security"; span [ class_ "absolute inset-0" ] [] ];
                  p
                    [ class_ "mt-1 txt-gray-600" ]
                    [ txt "Your customers’ data will be safe and secure" ];
                ];
            ];
          div
            [
              class_
                "group relative flex items-center gap-x-6 rounded-lg p-4 \
                 txt-sm leading-6 hover:bg-gray-50";
            ]
            [
              div
                [
                  class_
                    "flex h-11 w-11 flex-none items-center justify-center \
                     rounded-lg bg-gray-50 group-hover:bg-white";
                ]
                [
                  SVG.svg
                    [
                      class_ "h-6 w-6 txt-gray-600 group-hover:txt-indigo-600";
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
                          SVG.d
                            "M13.5 16.875h3.375m0 0h3.375m-3.375 0V13.5m0 \
                             3.375v3.375M6 10.5h2.25a2.25 2.25 0 \
                             002.25-2.25V6a2.25 2.25 0 00-2.25-2.25H6A2.25 \
                             2.25 0 003.75 6v2.25A2.25 2.25 0 006 10.5zm0 \
                             9.75h2.25A2.25 2.25 0 0010.5 18v-2.25a2.25 2.25 0 \
                             00-2.25-2.25H6a2.25 2.25 0 00-2.25 2.25V18A2.25 \
                             2.25 0 006 20.25zm9.75-9.75H18a2.25 2.25 0 \
                             002.25-2.25V6A2.25 2.25 0 0018 3.75h-2.25A2.25 \
                             2.25 0 0013.5 6v2.25a2.25 2.25 0 002.25 2.25z";
                        ]
                        [];
                    ];
                ];
              div
                [ class_ "flex-auto" ]
                [
                  a
                    [ href "#"; class_ "block font-semibold txt-gray-900" ]
                    [
                      txt "Integrations"; span [ class_ "absolute inset-0" ] [];
                    ];
                  p
                    [ class_ "mt-1 txt-gray-600" ]
                    [ txt "Connect with third-party tools" ];
                ];
            ];
          div
            [
              class_
                "group relative flex items-center gap-x-6 rounded-lg p-4 \
                 txt-sm leading-6 hover:bg-gray-50";
            ]
            [
              div
                [
                  class_
                    "flex h-11 w-11 flex-none items-center justify-center \
                     rounded-lg bg-gray-50 group-hover:bg-white";
                ]
                [
                  SVG.svg
                    [
                      class_ "h-6 w-6 txt-gray-600 group-hover:txt-indigo-600";
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
                          SVG.d
                            "M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 \
                             0h4.992m-4.993 0l3.181 3.183a8.25 8.25 0 \
                             0013.803-3.7M4.031 9.865a8.25 8.25 0 \
                             0113.803-3.7l3.181 3.182m0-4.991v4.99";
                        ]
                        [];
                    ];
                ];
              div
                [ class_ "flex-auto" ]
                [
                  a
                    [ href "#"; class_ "block font-semibold txt-gray-900" ]
                    [ txt "Automations"; span [ class_ "absolute inset-0" ] [] ];
                  p
                    [ class_ "mt-1 txt-gray-600" ]
                    [ txt "Build strategic funnels that will convert" ];
                ];
            ];
        ];
      div
        [ class_ "grid grid-cols-2 divide-x divide-gray-900/5 bg-gray-50" ]
        [
          a
            [
              href "#";
              class_
                "flex items-center justify-center gap-x-2.5 p-3 txt-sm \
                 font-semibold leading-6 txt-gray-900 hover:bg-gray-100";
            ]
            [
              SVG.svg
                [
                  class_ "h-5 w-5 flex-none txt-gray-400";
                  SVG.viewbox ~min_x:0 ~min_y:0 ~width:20 ~height:20;
                  SVG.fill "currentColor";
                  Aria.hidden true;
                ]
                [
                  SVG.path
                    [
                      SVG.fill "evenodd";
                      SVG.d
                        "M2 10a8 8 0 1116 0 8 8 0 01-16 0zm6.39-2.908a.75.75 0 \
                         01.766.027l3.5 2.25a.75.75 0 010 1.262l-3.5 \
                         2.25A.75.75 0 018 12.25v-4.5a.75.75 0 01.39-.658z";
                      (* SVG.clipRule "evenodd"; *)
                    ]
                    [];
                ];
              txt "Watch demo";
            ];
          a
            [
              href "#";
              class_
                "flex items-center justify-center gap-x-2.5 p-3 txt-sm \
                 font-semibold leading-6 txt-gray-900 hover:bg-gray-100";
            ]
            [
              SVG.svg
                [
                  class_ "h-5 w-5 flex-none txt-gray-400";
                  SVG.viewbox ~min_x:0 ~min_y:0 ~width:20 ~height:20;
                  SVG.fill "currentColor";
                  Aria.hidden true;
                ]
                [
                  SVG.path
                    [
                      SVG.fill "evenodd";
                      SVG.d
                        "M2 3.5A1.5 1.5 0 013.5 2h1.148a1.5 1.5 0 011.465 \
                         1.175l.716 3.223a1.5 1.5 0 01-1.052 \
                         1.767l-.933.267c-.41.117-.643.555-.48.95a11.542 \
                         11.542 0 006.254 \
                         6.254c.395.163.833-.07.95-.48l.267-.933a1.5 1.5 0 \
                         011.767-1.052l3.223.716A1.5 1.5 0 0118 \
                         15.352V16.5a1.5 1.5 0 01-1.5 1.5H15c-1.149 \
                         0-2.263-.15-3.326-.43A13.022 13.022 0 012.43 8.326 \
                         13.019 13.019 0 012 5V3.5z";
                      (* SVG.clipRule "evenodd"; *)
                    ]
                    [];
                ];
              txt "Contact sales";
            ];
        ];
    ]

let page =
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
            [ class_ "hidden lg:flex lg:gap-x-12" ]
            [
              div
                [ class_ "relative" ]
                [
                  button
                    [
                      type_ "button";
                      class_
                        "flex items-center gap-x-1 txt-sm font-semibold \
                         leading-6 txt-gray-900";
                      Aria.expanded false;
                    ]
                    [
                      txt "Product";
                      SVG.svg
                        [
                          class_ "h-5 w-5 flex-none txt-gray-400";
                          SVG.viewbox ~min_x:0 ~min_y:0 ~width:20 ~height:20;
                          SVG.fill "currentColor";
                          Aria.hidden true;
                        ]
                        [
                          SVG.path
                            [
                              SVG.fill "evenodd";
                              SVG.d
                                "M5.23 7.21a.75.75 0 011.06.02L10 \
                                 11.168l3.71-3.938a.75.75 0 111.08 1.04l-4.25 \
                                 4.5a.75.75 0 01-1.08 0l-4.25-4.5a.75.75 0 \
                                 01.02-1.06z";
                              (* SVG.clipRule "evenodd"; *)
                            ]
                            [];
                        ];
                    ];
                ];
              header_link "Features";
              header_link "Marketplace";
              header_link "Company";
            ];
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
      (* Mobile menu; show/hide based on menu open state. *)
      div
        [ class_ "lg:hidden"; role `dialog; Aria.modal ]
        [
          (* Background backdrop; show/hide based on slide-over state. *)
          div [ class_ "fixed inset-0 z-10" ] [];
          div
            [
              class_
                "fixed inset-y-0 right-0 z-10 w-full overflow-y-auto bg-white \
                 px-6 py-6 sm:max-w-sm sm:ring-1 sm:ring-gray-900/10";
            ]
            [
              div
                [ class_ "flex items-center justify-between" ]
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
                  button
                    [
                      type_ "button";
                      class_ "-m-2.5 rounded-md p-2.5 txt-gray-700";
                    ]
                    [
                      span [ class_ "sr-only" ] [ txt "Close menu" ];
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
                              SVG.d "M6 18L18 6M6 6l12 12";
                            ]
                            [];
                        ];
                    ];
                ];
              div
                [ class_ "mt-6 flow-root" ]
                [
                  div
                    [ class_ "-my-6 divide-y divide-gray-500/10" ]
                    [
                      div
                        [ class_ "space-y-2 py-6" ]
                        [
                          div
                            [ class_ "-mx-3" ]
                            [
                              button
                                [
                                  type_ "button";
                                  class_
                                    "flex w-full items-center justify-between \
                                     rounded-lg py-2 pl-3 pr-3.5 txt-base \
                                     font-semibold leading-7 txt-gray-900 \
                                     hover:bg-gray-50";
                                  Aria.controls "disclosure-1";
                                  Aria.expanded false;
                                ]
                                [
                                  txt "Product";
                                  (*
                  Expand/collapse icon; toggle classes based on menu open state.

                  Open: "rotate-180"; Closed: ""
                *)
                                  SVG.svg
                                    [
                                      class_ "h-5 w-5 flex-none";
                                      SVG.viewbox ~min_x:0 ~min_y:0 ~width:20
                                        ~height:20;
                                      SVG.fill "currentColor";
                                      Aria.hidden true;
                                    ]
                                    [
                                      SVG.path
                                        [
                                          SVG.fill "evenodd";
                                          SVG.d
                                            "M5.23 7.21a.75.75 0 011.06.02L10 \
                                             11.168l3.71-3.938a.75.75 0 111.08 \
                                             1.04l-4.25 4.5a.75.75 0 01-1.08 \
                                             0l-4.25-4.5a.75.75 0 01.02-1.06z";
                                          (* SVG.clipRule "evenodd"; *)
                                        ]
                                        [];
                                    ];
                                ];
                              (* 'Product' sub-menu; show/hide based on menu state. *)
                              div
                                [ class_ "mt-2 space-y-2"; id "disclosure-1" ]
                                [
                                  a
                                    [
                                      href "#";
                                      class_
                                        "block rounded-lg py-2 pl-6 pr-3 \
                                         txt-sm font-semibold leading-7 \
                                         txt-gray-900 hover:bg-gray-50";
                                    ]
                                    [ txt "Analytics" ];
                                  a
                                    [
                                      href "#";
                                      class_
                                        "block rounded-lg py-2 pl-6 pr-3 \
                                         txt-sm font-semibold leading-7 \
                                         txt-gray-900 hover:bg-gray-50";
                                    ]
                                    [ txt "Engagement" ];
                                  a
                                    [
                                      href "#";
                                      class_
                                        "block rounded-lg py-2 pl-6 pr-3 \
                                         txt-sm font-semibold leading-7 \
                                         txt-gray-900 hover:bg-gray-50";
                                    ]
                                    [ txt "Security" ];
                                  a
                                    [
                                      href "#";
                                      class_
                                        "block rounded-lg py-2 pl-6 pr-3 \
                                         txt-sm font-semibold leading-7 \
                                         txt-gray-900 hover:bg-gray-50";
                                    ]
                                    [ txt "Integrations" ];
                                  a
                                    [
                                      href "#";
                                      class_
                                        "block rounded-lg py-2 pl-6 pr-3 \
                                         txt-sm font-semibold leading-7 \
                                         txt-gray-900 hover:bg-gray-50";
                                    ]
                                    [ txt "Automations" ];
                                  a
                                    [
                                      href "#";
                                      class_
                                        "block rounded-lg py-2 pl-6 pr-3 \
                                         txt-sm font-semibold leading-7 \
                                         txt-gray-900 hover:bg-gray-50";
                                    ]
                                    [ txt "Watch demo" ];
                                  a
                                    [
                                      href "#";
                                      class_
                                        "block rounded-lg py-2 pl-6 pr-3 \
                                         txt-sm font-semibold leading-7 \
                                         txt-gray-900 hover:bg-gray-50";
                                    ]
                                    [ txt "Contact sales" ];
                                ];
                            ];
                          a
                            [
                              href "#";
                              class_
                                "-mx-3 block rounded-lg px-3 py-2 txt-base \
                                 font-semibold leading-7 txt-gray-900 \
                                 hover:bg-gray-50";
                            ]
                            [ txt "Features" ];
                          a
                            [
                              href "#";
                              class_
                                "-mx-3 block rounded-lg px-3 py-2 txt-base \
                                 font-semibold leading-7 txt-gray-900 \
                                 hover:bg-gray-50";
                            ]
                            [ txt "Marketplace" ];
                          a
                            [
                              href "#";
                              class_
                                "-mx-3 block rounded-lg px-3 py-2 txt-base \
                                 font-semibold leading-7 txt-gray-900 \
                                 hover:bg-gray-50";
                            ]
                            [ txt "Company" ];
                        ];
                      div
                        [ class_ "py-6" ]
                        [
                          a
                            [
                              href "#";
                              class_
                                "-mx-3 block rounded-lg px-3 py-2.5 txt-base \
                                 font-semibold leading-7 txt-gray-900 \
                                 hover:bg-gray-50";
                            ]
                            [ txt "Log in" ];
                        ];
                    ];
                ];
            ];
        ];
    ]
