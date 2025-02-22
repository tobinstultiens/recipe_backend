open Dream_html
open HTML

let headElements =
  head []
    [
      title [] "Recipe Manager";
      script [ src "https://unpkg.com/htmx.org@2.0.3" ] "";
      link [ rel "stylesheet"; href "https://unpkg.com/mvp.css" ];
    ]

let view = html [] [ headElements; null [ p [ class_ "" ] [ txt "Hello" ]; p [] [ txt "World" ] ] ]
let greet name = p [ id "greet-%s" name ] [ txt "Hello, %s!" name ]

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.livereload
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream_html.respond view);
         Dream.get "/echo/:word" (fun request -> Dream_html.respond (greet (Dream.param request "word")));
       ]
