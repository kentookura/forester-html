open Dream_html
open HTML

let top =
  div
    [ class_ "sm:mx-auto sm:w-full sm:max-w-sm" ]
    [
      img
        [
          src "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=500";
          class_ "mx-auto h-10 w-auto";
        ];
      h2
        [
          class_
            "mt-10 text-center text-2xl font-bold leading-9 tracking-tight \
             text-white";
        ]
        [];
    ]

let form_field ?ph c lbl =
  div []
    [
      label
        [ for_ c; class_ "block text-sm font-medium leading-6 text-gray-900" ]
        [ txt lbl ];
      div
        [ class_ "mt-2" ]
        [
          input
            [
              (match ph with Some p -> placeholder "%s" p | None -> null_);
              type_ c;
              name c;
              id c;
              class_
                "block w-full rounded-md border-0 py-1.5 text-gray-900 \
                 shadow-sm ring-1 ring-inset ring-gray-300 \
                 placeholder:text-gray-400 focus:ring-2 focus:ring-inset \
                 focus:ring-indigo-600 sm:text-sm sm:leading-6";
            ];
        ];
    ]

let login_form lbl endpoint request =
  div
    [ class_ "mt-10 sm:mx-auto sm:w-full sm:max-w-sm" ]
    [
      form
        [
          method_ `POST;
          Hx.post endpoint;
          Hx.trigger "submit";
          class_ "space-y-6";
        ]
        [
          csrf_tag request;
          form_field ?ph:(Some "you@example.com") "email" "Email";
          form_field "password" "Password";
          div []
            [
              button
                [
                  type_ "submit";
                  class_
                    "flex w-full justify-center rounded-md bg-indigo-500 px-3 \
                     py-1.5 text-sm font-semibold leading-6 text-white \
                     shadow-sm hover:bg-indigo-400 focus-visible:outline \
                     focus-visible:outline-2 focus-visible:outline-offset-2 \
                     focus-visible:outline-indigo-500";
                ]
                [ txt "%s" lbl ];
            ];
        ];
    ]

let not_a_member =
  p
    [ class_ "mt-10 text-center text-sm text-gray-400" ]
    [
      txt "Not a member? ";
      a
        [
          href "/signup";
          class_ "font-semibold leading-6 text-indigo-400 hover:text-indigo-300";
        ]
        [ txt "Sign up now!" ];
    ]

let login_page req =
  div
    [ class_ "flex min-h-full flex-col justify-center px-6 py-12 lg:px-8" ]
    [ top; login_form "Sign in" "/login" req; not_a_member ]

let signup_page req =
  div
    [ class_ "flex min-h-full flex-col justify-center px-6 py-12 lg:px-8" ]
    [ top; login_form "Sign up" "/signup" req ]
