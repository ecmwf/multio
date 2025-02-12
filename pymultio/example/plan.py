# Construct a plan for a multio server

import multio

Plan = multio.plans.Plan(
    name="example",
    actions=[
        multio.plans.Print(stream="cout", prefix="DEBUG"),
        multio.plans.Statistics(operations=["average"], output_frequency="5h"),
        multio.plans.Sink(multio.plans.File("output")),
    ],
)

with multio.MultioPlan(Plan):
    with multio.Multio() as mio:
        mio.write_field(None, [1.0, 2.0])
