#import "@preview/fletcher:0.5.8" as fletcher: diagram, edge, node
#set page(width: auto, height: auto, margin: 5mm, fill: white)
#set text(8pt)

#let bent-edge(from, to, ..args) = {
  let midpoint = (from, 50%, to)
  let vertices = (
    from,
    (from, "|-", midpoint),
    (midpoint, "-|", to),
    to,
  )
  edge(..vertices, "-|>", ..args)
}

#let straight-edge(from, to, ..args) = {
  edge(from, to, "-|>", ..args)
}

#diagram(
  spacing: (0pt, 8mm),
  node-corner-radius: 0pt,
  edge-corner-radius: none,
  node-stroke: 1pt,
  node-shape: rect,
  node-inset: 5pt,
  edge-stroke: 1pt,
  mark-scale: 75%,

  node((0, 0), [Invited to participate\ *6,056*], name: <invited>, width: 30mm),
  node((0, 1), [Phone pre-screening\ *2,819*], name: <pre-screened>, width: 30mm),
  node((0, 2), [In-clinic screening\ *975*], name: <screened>, width: 30mm),
  node((0, 3), [Randomised\ *972*], name: <randomised>, width: 30mm),
  node((-1, 4), [Assigned aP-only\ *486*], name: <aP>, width: 30mm),
  node((1, 4), [Assigned mixed wP/aP\ *486*], name: <wP>, width: 30mm),

  node(
    (-1, 5),
    align(left)[
      #grid(
        columns: 2,
        rows: auto,
        column-gutter: 2pt,
        row-gutter: 1mm,
        align: (left, right),
        inset: 0pt,
        [Completed 12-month visit], [*474*],
        [Missed visit], [*12*],
        [- Lost-to follow-up], [*4*],
        [- Withdrew from study], [*3*],
        [- Did not attend], [*5*],
      )],
    name: <ap-12month>,
    width: 40mm,
  ),
  node(
    (1, 5),
    align(left)[
      #grid(
        columns: 2,
        rows: auto,
        column-gutter: 2pt,
        row-gutter: 1mm,
        align: (left, right),
        inset: 0pt,
        [Completed 12-month visit], [*473*],
        [Missed visit], [*13*],
        [- Lost-to follow-up], [*3*],
        [- Withdrew from study], [*4*],
        [- Did not attend], [*6*],
      )],
    name: <wp-12month>,
    width: 40mm,
  ),

  node(
    (-1, 6),
    align(left)[
      #grid(
        columns: 2,
        rows: auto,
        column-gutter: 2pt,
        row-gutter: 1mm,
        align: (left, right),
        inset: 0pt,
        [Completed 18-month visit], [*470*],
        [Missed visit], [*16*],
        [- Lost-to follow-up], [*10*],
        [- Withdrew from study], [*4*],
        [- Did not attend], [*2*],
      )],
    name: <ap-18month>,
    width: 40mm,
  ),
  node(
    (1, 6),
    align(left)[
      #grid(
        columns: 2,
        rows: auto,
        column-gutter: 2pt,
        row-gutter: 1mm,
        align: (left, right),
        inset: 0pt,
        [Completed 18-month visit], [*470*],
        [Missed visit], [*16*],
        [- Lost-to follow-up], [*9*],
        [- Withdrew from study], [*5*],
        [- Did not attend], [*2*],
      )],
    name: <wp-18month>,
    width: 40mm,
  ),

  node(
    (-1, 7),
    grid(
      columns: 2,
      rows: auto,
      column-gutter: 2pt,
      row-gutter: 1mm,
      align: (left, right),
      inset: 0pt,
      [Completed scheduled SPT], [*472*],
      [Any unscheduled SPT], [*24*],
      [Any oral food challenge], [*29*],
    ),
    name: <ap-assessments>,
    width: 40mm,
  ),
  node(
    (1, 7),
    grid(
      columns: 2,
      rows: auto,
      column-gutter: 2pt,
      row-gutter: 1mm,
      align: (left, right),
      inset: 0pt,
      [Completed scheduled SPT], [*471*],
      [Any unscheduled SPT], [*31*],
      [Any oral food challenge], [*24*],
    ),
    name: <wp-assessments>,
    width: 40mm,
  ),

  straight-edge(<invited>, <pre-screened>),
  straight-edge(<pre-screened>, <screened>),
  straight-edge(<screened>, <randomised>),
  bent-edge(<randomised>, <aP>),
  bent-edge(<randomised>, <wP>),
  straight-edge(<aP>, <ap-12month>),
  straight-edge(<wP>, <wp-12month>),
  straight-edge(<ap-12month>, <ap-18month>),
  straight-edge(<wp-12month>, <wp-18month>),
  straight-edge(<ap-18month>, <ap-assessments>),
  straight-edge(<wp-18month>, <wp-assessments>),
)
