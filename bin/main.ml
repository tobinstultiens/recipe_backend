open Dream_html
open HTML

let headTailwind =
  head []
    [
      title [] "Recipe Manager";
      script [ src "https://cdn.tailwindcss.com" ] "";
      script [ src "https://unpkg.com/htmx.org@2.0.3" ] "";
      script [] "tailwind config = { config: './css/tailwindcss-config.js' }";
    ]

let view = html [] [ headTailwind; null [ p [ class_ "" ] [ txt "Hello" ]; p [] [ txt "World" ] ] ]
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
