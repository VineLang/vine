#import "/lib.typ": *
#import hyptyp: t

#import "img.typ"

#include "deps/typsitter-langs/langs.typ"

#show: typsitter.register(pdf-theme, html_support: true)
#show raw: set block(inset: 10pt, radius: 2pt, width: 100%, breakable: false)

#show: it => context if target() == "paged" {
  set page(numbering: "1")
  show raw.where(block: false): box.with(outset: (y: 2pt), inset: (x: 2pt), radius: 2pt, fill: c5)
  it
} else {
  it
}

#set text(font: "Atkinson Hyperlegible", size: 11pt)
#show raw: set text(font: "Atkinson Hyperlegible Mono", size: 11pt)

#show ref: it => {
  let el = it.element
  if target() != "html" and el != none {
    show: underline.with(stroke: ca)
    show: link.with(el.location())
    it.supplement
    " (page "
    numbering(
      "1",
      ..counter(page).at(el.location())
    )
    ")"
  } else {
    it
  }
}

#hyptyp.resource("/theme.css", read("theme.css"))
#hyptyp.resource("/typsitter.css", {
  typsitter.theme-css(dark-theme)
  "@media (prefers-color-scheme: light) {\n"
  typsitter.theme-css(light-theme)
  "\n}"
})

#hyptyp.resource("/index.html", slug: "/", read("landing.html"))

#img.resources

#show: hyptyp.show-site(
  root: "docs.typ",

  root-slug: "/docs/",

  path-to-slug: site => path => "/docs" + (hyptyp.defaults.path-to-slug)(site)(path),

  sidebar-header: site => _ => [
    #t.a(class: "logo", href: "/")[#t.img(class: "logo", src: "/logo.svg")]
  ],

  head-extra: site => _ => {
    t.link(rel: "stylesheet", href: "/typsitter.css")
    t.link(rel: "stylesheet", href: "/theme.css")
    img.icons
  }
)
