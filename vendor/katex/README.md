KaTeX 0.18.7, the stylesheet and the woff2 fonts only (MIT; see LICENSE).

The rules on /alignment/ are typeset with KaTeX at build time -- by hand, with
`katex.renderToString`, into static HTML -- so the page ships no JavaScript
for it and fetches nothing from anywhere but this host. The browser picks the
woff2 face from each `src` list and never asks for the woff or ttf ones, so
those are not here.
