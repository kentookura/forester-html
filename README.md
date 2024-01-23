# Quickstart

Put some trees in `./static` and run `dune exec --watch forester_html`

# What

An alternative rendering architecture for forester which uses 

- [Dream](https://aantron.github.io/dream/) 
- [Dream-html](https://ocaml.org/p/dream-html/2.0.0/doc/index.html) 
- [HTMX](https://htmx.org/) 

instead XML and XSLT.

<details>
<summary>Aside</summary>
Coincidentally, this repo kind of functions as an ocaml implementation
of a development server for forester:
running `dune exec --watch forester_html` shows the compilation errors and
hosts the forest (although currently not everything is being rendered).
The downside is obviously that the functionality is not "installable",
meaning you need to run `dune` in the repo for this to work.
</details>

# Why

HTMX extends HTML by allowing any element to perform HTTP transactions and
allowing the programmer to replace any element in the DOM instead of the entire
page. This means we can just write a bunch of functions `f: forest -> HTML` and
use them in the UI in a flexible manner, allowing for more interactivity. 

We can send diagnostics rendered to HTML to the browser via [SSE](https://htmx.org/extensions/server-sent-events) and [Websockets](https://aantron.github.io/dream/#websockets)

Furthermore, it will allow us to get and render trees at runtime, which is useful
if I want to transclude trees from other forests. 

# Possibilities

- Managing forests from the frontend via http calls (e.g. "create new tree"-button)
 
- cross-forest transclusions should (!?) be easy?

- Teacher-student interactions

- Showing diagnostics in the browser at relevant locations.

# Blockers

I haven't figured out if it is possible to keep the server running
when a fatal Asai error occurs. We want to report errors to the browser
and keep running.

# References
[Hypermedia Systems by Carson Gross, Adam Stepinski and Deniz Akşimşek](https://hypermedia.systems/)
