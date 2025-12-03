
#import "deps/hyptyp/hyptyp.typ"
#import "deps/typsitter/typsitter.typ"

#import hyptyp: t

#let theme(theme, fg, fgl, bg) = {
  let comment = color.mix(space: rgb, (fg, 50%), (bg, 50%))
  let keyword = color.mix(space: oklab, (theme, 80%), (fg, 20%));
  let vague = fgl;
  let special = color.mix(space: oklab, (theme, 50%), (fg, 40%), (white, 10%))
  let string = color.mix(space: oklab, (theme, 60%), (fg, 30%), (blue, 10%))

  (
    "fg": fg,
    "bg": bg,
    "string": string,
    "comment": comment,
    "keyword": keyword,
    "operator": keyword,
    "constant": special,
    "variable.builtin": special,
    "type": special,
    "function": special,
    "punctuation": vague,
    "namespace": vague,
  )
}

#let theme-bright = rgb("#46c676")
#let theme-dim = rgb("#169646")

#let white = rgb("#d6dde0")
#let dawn = rgb("#bdc3c7")
#let dusk = rgb("#252830")
#let black = rgb("#151820")

#let dawn-white = color.mix(space: rgb, (dawn, 50%), (white, 50%))
#let dusk-black = color.mix(space: rgb, (dusk, 50%), (black, 50%))

#let dawn-grey = color.mix(space: rgb, (dawn, 75%), (black, 25%))
#let dusk-grey = color.mix(space: rgb, (dusk, 75%), (white, 25%))

#let dark-theme = theme(theme-bright, dawn, dawn-grey, dusk-black);
#let light-theme = theme(theme-dim, dusk, dusk-grey, dawn-white);
#let pdf-theme = theme(theme-dim, dusk, dusk-grey, dawn-white);

#let issue = number => link("https://github.com/VineLang/vine/issues/" + str(number))[\##number]

#let vi_ = (prefix, suffix, raw) => {
  set box(outset: (y: 2pt), inset: (x: 2pt), radius: 2pt)
  set text(font: "Atkinson Hyperlegible Mono")
  typsitter.render(
    pdf-theme,
    block: false, lang: "vine",
    html_support: true,
    prefix: prefix, suffix: suffix,
    raw.text,
  )
}

#let vi = vi_.with("", "")
#let expr = vi_.with("const x =", ";")
#let ty = vi_.with("type x = ", ";")
#let pat = vi_.with("fn x(", ") {}")
#let fn = vi_.with("fn ", "() {}")
#let op = vi_.with("1 ", " 1")
#let vstr = vi_.with("\"", "\"")

#let api_anchor_link = (name) => {
  t.a(name: name, href: "#" + name, class: "anchor")[#raw(name)]
}

#let src = (file, line: none) => {
  let url = "https://github.com/VineLang/vine/tree/main/" + file
  let label = file
  if line != none {
    url += "#L" + str(line)
    label += ":" + str(line)
  }
  t.a(class: "src", href: url)[#raw(label)]
}

#let todo-counter = counter("todo")
#let todo = (desc) => [
  #context {
    let id = "todo-" + str(todo-counter.get().at(0))
    [#t.span(class: "todo", id: id)[\[TODO: #desc\]] #metadata((id: id, desc: desc)) <todo>]
  }
  #todo-counter.step()
]

#let todos = context {
  let todos = query(<todo>)
  if todos != () {
    [=== TODO]
    list(
      ..for todo in todos {
       ([
        #link(hyptyp.slug.at(todo.location()) + "#" + todo.value.id)[#todo.value.desc]
        (#raw(hyptyp.slug.at(todo.location())))
      ],)
      }
    )
  }
}

