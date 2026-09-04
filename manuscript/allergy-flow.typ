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

#let _node = node
#let node(pos, label, ..args) = _node(
  pos,
  label,
  width: 40mm,
  height: 15mm,
  ..args,
)

#diagram(
  spacing: (8mm, 10mm),
  node-corner-radius: 0pt,
  edge-corner-radius: none,
  node-stroke: 1pt,
  node-shape: rect,
  node-inset: 5pt,
  edge-stroke: 1pt,
  mark-scale: 60%,

  node((0, 0), [Parent reported food allergy.], name: <1>),
  node((0, 1), [History reviewed by allergist.], name: <2>),
  node(
    (-1, 2),
    [History not indicative of IgE-mediated food allergy.],
    name: <3>,
  ),
  node(
    (1, 2),
    [History indicative of IgE-mediated food allergy.],
    name: <4>,
  ),
  node(
    (1, 3),
    [Unscheduled visit (SPT?) at allergist discretion.],
    name: <5>,
  ),
  node((2, 3), [], stroke: 0pt),
  node((0, 4), [Scheduled SPT at 12-months], name: <6>),
  node((-1, 5), [Positive SPT (>1mm)], name: <7>),
  node((1, 5), [Negative SPT (>1mm)], name: <8>),
  node((1, 6), [Not allergic], name: <9>, fill: luma(200)),
  node((-1, 6), [History and SPT reviewed by allergist], name: <10>),
  node((-2, 7), [No IgE allergy history], name: <11>),
  node((-1, 7), [Inconclusive allergy history], name: <12>),
  node((0, 7), [Highly suspected IgE allergy history], name: <13>),
  node((1, 7), [History of anaphylaxis], name: <14>),
  node((-2, 8), [Not allergic], name: <15>, fill: luma(200)),
  node((1, 8), [Meets unequivocal IgE-mediated allergy (anaphylaxis)], name: <16>, fill: luma(200)),
  node((-0.5, 8), [Book for oral food challenge (OFC)], name: <17>),
  node((-1.5, 9), [Parent declines OFC], name: <18>),
  node((-0.5, 9), [Positive OFC], name: <19>),
  node((0.5, 9), [Negative OFC], name: <20>),
  node(
    (-1.5, 10),
    [Use available history to determine if highly probable per PRACTALL criteria],
    name: <21>,
  ),
  node(
    (-0.5, 10),
    [Meets unequivocal IgE-mediated allergy (OFC)],
    name: <22>,
    fill: luma(200),
  ),
  node(
    (-0.5, 11),
    [If had previous highly suspected exposure also meets highly probable food allergy],
    name: <25>,
    fill: luma(200),
  ),
  node(
    (0.5, 10),
    [Clinician considers previous IgE-mediated allergy has since resolved],
    name: <23>,
  ),
  node(
    (1.5, 10),
    [Clinician considers unlikely to have ever had IgE-mediated allergy],
    name: <24>,
  ),
  node(
    (0.5, 11),
    [Meets highly probable food allergy],
    name: <26>,
    fill: luma(200),
  ),
  node(
    (1.5, 11),
    [Not allergic],
    name: <27>,
    fill: luma(200),
  ),

  straight-edge(<1>, <2>),
  bent-edge(<2>, <3>),
  bent-edge(<2>, <4>),
  straight-edge(<4>, <5>),
  bent-edge(<3>, <6>),
  edge(<5>, (0, 3)),
  bent-edge(<6>, <7>),
  bent-edge(<6>, <8>),
  straight-edge(<7>, <10>),
  straight-edge(<8>, <9>),
  bent-edge(<10>, <11>),
  straight-edge(<10>, <12>),
  bent-edge(<10>, <13>),
  bent-edge(<10>, <14>),
  straight-edge(<11>, <15>),
  straight-edge(<14>, <16>),
  bent-edge(<12>, <17>),
  bent-edge(<13>, <17>),
  bent-edge(<17>, <18>),
  straight-edge(<17>, <19>),
  bent-edge(<17>, <20>),
  straight-edge(<18>, <21>),
  straight-edge(<19>, <22>),
  straight-edge(<20>, <23>),
  bent-edge(<20>, <24>),
  straight-edge(<22>, <25>),
  straight-edge(<23>, <26>),
  straight-edge(<24>, <27>),
)
