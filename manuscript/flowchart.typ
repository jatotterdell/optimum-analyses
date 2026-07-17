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

#let node-grid(..args) = {
  grid(
    columns: (90%, auto),
    rows: auto,
    column-gutter: 2pt,
    row-gutter: 1mm,
    align: (left, right),
    inset: 0pt,
    ..args
  )
}

#let node-width = 40mm

#diagram(
  spacing: (0pt, 8mm),
  node-corner-radius: 0pt,
  edge-corner-radius: none,
  node-stroke: 1pt,
  node-shape: rect,
  node-inset: 5pt,
  edge-stroke: 1pt,
  mark-scale: 60%,

  node((0, 0), [Invited to participate\ *6,056*], name: <invited>, width: node-width),
  node(
    (-1, 0.5),
    align(left)[
      #node-grid(
        [Declined follow-up],
        [*1,591*],
        [Lost to follow-up],
        [*1,646*],
      )
    ],
    name: <declined-1>,
    width: 35mm,
  ),

  node((0, 1), [Phone pre-screening\ *2,819*], name: <pre-screened>, width: node-width),
  node(
    (-1, 1.5),
    align(left)[
      #node-grid(
        [Declined to continue],
        [*1,586*],
        [Ineligible],
        [*258*],
      )
    ],
    name: <declined-2>,
    width: 35mm,
  ),

  node((0, 2), [In-clinic screening\ *975*], name: <screened>, width: node-width),
  node(
    (-1, 2.5),
    align(left)[
      #node-grid(
        [Ineligible],
        [*3*],
      )
    ],
    name: <declined-3>,
    width: 35mm,
  ),

  node((0, 3), [Randomised\ *972*], name: <randomised>, width: node-width),
  node((-1, 4), [Assigned aP-only\ *486*], name: <aP>, width: node-width),
  node((1, 4), [Assigned mixed wP/aP\ *486*], name: <wP>, width: node-width),

  node(
    (-1, 5),
    align(left)[
      #node-grid(
        [Completed 12-month visit],
        [*474*],
        [Missed visit],
        [*12*],
        [- Lost-to follow-up],
        [*4*],
        [- Withdrew from study],
        [*3*],
        [- Did not attend],
        [*5*],
      )],
    name: <ap-12month>,
    width: node-width,
  ),
  node(
    (1, 5),
    align(left)[
      #node-grid(
        [Completed 12-month visit],
        [*473*],
        [Missed visit],
        [*13*],
        [- Lost-to follow-up],
        [*3*],
        [- Withdrew from study],
        [*4*],
        [- Did not attend],
        [*6*],
      )],
    name: <wp-12month>,
    width: node-width,
  ),

  node(
    (-1, 6),
    align(left)[
      #node-grid(
        [Completed 18-month visit],
        [*470*],
        [Missed visit],
        [*16*],
        [- Lost-to follow-up],
        [*10*],
        [- Withdrew from study],
        [*4*],
        [- Did not attend],
        [*2*],
      )],
    name: <ap-18month>,
    width: node-width,
  ),
  node(
    (1, 6),
    align(left)[
      #node-grid(
        [Completed 18-month visit],
        [*470*],
        [Missed visit],
        [*16*],
        [- Lost-to follow-up],
        [*9*],
        [- Withdrew from study],
        [*5*],
        [- Did not attend],
        [*2*],
      )],
    name: <wp-18month>,
    width: node-width,
  ),

  node(
    (-1, 7),
    align(left)[
      #node-grid(
        [Completed scheduled SPT],
        [*472*],
        [Any unscheduled SPT],
        [*24*],
        [Any oral food challenge],
        [*29*],
      )],
    name: <ap-assessments>,
    width: node-width,
  ),
  node(
    (1, 7),
    align(left)[
      #node-grid(
        [Completed scheduled SPT],
        [*471*],
        [Any unscheduled SPT],
        [*31*],
        [Any oral food challenge],
        [*24*],
      )],
    name: <wp-assessments>,
    width: node-width,
  ),

  straight-edge(<invited>, <pre-screened>),
  straight-edge((rel: (0, 0.5), to: <invited>), <declined-1.east>),
  straight-edge(<pre-screened>, <screened>),
  straight-edge((rel: (0, 0.5), to: <pre-screened>), <declined-2.east>),
  straight-edge(<screened>, <randomised>),
  straight-edge((rel: (0, 0.5), to: <screened>), <declined-3.east>),
  bent-edge(<randomised>, <aP>),
  bent-edge(<randomised>, <wP>),
  straight-edge(<aP>, <ap-12month>),
  straight-edge(<wP>, <wp-12month>),
  straight-edge(<ap-12month>, <ap-18month>),
  straight-edge(<wp-12month>, <wp-18month>),
  straight-edge(<ap-18month>, <ap-assessments>),
  straight-edge(<wp-18month>, <wp-assessments>),
)
