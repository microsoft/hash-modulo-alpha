import math
from typing import Dict

import argparse
import numpy as np
import matplotlib
import matplotlib.pyplot as plt


def make_plot(filename: str, descr: str, bert_plot: bool, results: Dict[str, str]):
    # Some elements may benefit from slightly larger font sizes - define these here.
    LEGEND_FONT_SIZE = 14
    AXIS_LABEL_FONT_SIZE = 15

    # Get colors from the default color map.
    prop_cycle = plt.rcParams["axes.prop_cycle"]
    colors = prop_cycle.by_key()["color"]

    # Mapping from algorithms to colors.
    color_map = {
        "LN": colors[0],
        "Ours": colors[2],
        "DeBruijn": colors[1],
        "Structural": colors[3],
    }

    # Mapping from algorithms to marker shapes.
    marker_map = {"LN": "+", "Ours": "o", "DeBruijn": "^", "Structural": "s"}

    # Mapping from algorithms to how they are shown in the legend.
    display_name = {
        "LN": "Locally Nameless",
        "Ours": "Ours",
        "DeBruijn": "De Bruijn*",
        "Structural": "Structural*",
    }

    for algo, data_file in results.items():
        lines = [line.rstrip().split() for line in open(data_file, "rt")]
        lines = sorted([[float(x) for x in line] for line in lines])

        [xs, ys] = zip(*lines)

        marker = marker_map[algo]
        color = color_map[algo]

        if bert_plot:
            # For BERT examples, use markers connected by line segments.
            plt.plot(
                xs,
                ys,
                label=display_name[algo],
                markersize=6.0,
                color=color,
                markerfacecolor="none" if marker not in ["+", "x"] else color,
                marker=marker,
                markeredgewidth=0.6,
                linewidth=0.3,
            )
        else:
            # Since we generate too much data, take every 4th result.
            xs = [xs[i] for i in range(0, len(xs), 4)]
            ys = [ys[i] for i in range(0, len(ys), 4)]

            # Non-BERT examples have a bit of noise, so leave the markers unconnected.
            plt.scatter(
                xs,
                ys,
                label=display_name[algo],
                s=30.0,
                edgecolors=color,
                facecolors="none" if marker not in ["+", "x"] else color,
                marker=marker,
                linewidth=0.6,
            )

    # Define axis limits, anchor point for the arrows, and hand-picked shifts (explained later).
    if bert_plot:
        xlim = (1470, 24500)
        ylim = (9e-7, 1.0)

        anchor_x, anchor_y = 2700, 3e-3
        complexity_annotations_x_length = 8

        shifts_x = [1.44, 1.1, 1.11]
        shifts_y = [2.2, 1.2, 1.0]
    else:
        xlim = (1, 2e7)
        ylim = (1e-8, 1)

        anchor_x, anchor_y = 1.5, 1e-5
        complexity_annotations_x_length = 150.0

        shifts_x = [0.87, 0.29, 0.46]
        shifts_y = [1.1, 0.9, 1.05]

    plt.xlim(xlim)
    plt.ylim(ylim)

    # Define the functions we wish to show, together with their LaTeX renderings.
    funcs = [
        (lambda x: x, "n"),
        (lambda x: x * math.log2(x) ** 2, "n \log^2 n"),
        (lambda x: x ** 2 * math.log2(x), "n^2 \log n"),
    ]

    for (func, func_latex), shift_x, shift_y in zip(funcs, shifts_x, shifts_y):
        # To gauge how `func` grows, we evaluate it in two points `x_start` and `x_end`. Functions
        # of the form `x^k` are a straight line on a log-log plot, so the exact evaluation positions
        # do not matter. For other functions, what the arrows show is *approximate* growth.
        x_start = 1e1
        x_end = x_start * complexity_annotations_x_length

        xs = [x_start, x_end]
        ys = [func(x) for x in xs]

        x_ratio = anchor_x / xs[0]
        y_ratio = anchor_y / ys[0]

        xs = [x * x_ratio for x in xs]
        ys = [y * y_ratio for y in ys]

        # Use a function that plots text and an arrow, but set text to empty, to get just the arrow.
        plt.annotate(
            "",
            xy=(xs[1], ys[1]),
            xytext=(xs[0], ys[0]),
            arrowprops=dict(
                width=0.1, headwidth=5, headlength=5, color="grey", shrink=0.0
            ),
        )

        # Compute the slope of the arrow in logspace.
        ratio = (math.log(ys[1]) - math.log(ys[0])) / (
            math.log(xs[1]) - math.log(xs[0])
        )
        ratio /= (math.log(ylim[1]) - math.log(ylim[0])) / (
            math.log(xlim[1]) - math.log(xlim[0])
        )

        # Even though we did so much work computing the exact angle above, matplotlib ends up
        # rendering the text at an angle which is just a tiny bit off. Adjusting this manually here.
        ratio *= 0.74

        # Convert slope to angle.
        angle = math.atan(ratio) / math.pi * 180.0

        # Compute where the text should start. Again, theoretically correct positioning ends up off,
        # so apply hand-designed offsets `shift_x` and `shift_y` to fix things up.
        text_xpos = x_end * (complexity_annotations_x_length ** -0.32) * shift_x
        text_ypos = func(text_xpos) * math.exp(ratio * shift_y)

        text_xpos *= x_ratio
        text_ypos *= y_ratio

        # Finally, render the LaTeX.
        plt.text(
            text_xpos,
            text_ypos,
            "$\mathcal{O}({" + func_latex + "})$",
            rotation=angle,
            color="grey",
        )

    plt.xscale("log")
    plt.yscale("log")

    plt.xlabel(f"Number of nodes ({descr})", fontsize=AXIS_LABEL_FONT_SIZE)
    plt.ylabel(
        "Time taken to hash all subexpressions (s)", fontsize=AXIS_LABEL_FONT_SIZE
    )

    legend = plt.legend(loc=4, fontsize=LEGEND_FONT_SIZE)

    if not bert_plot:
        for handle in legend.legendHandles:
            handle.set_sizes([60.0])

    plt.savefig(filename, bbox_inches="tight")
    plt.clf()


def main():
    parser = argparse.ArgumentParser(
        description="Generate nice plots from *.dat files."
    )
    parser.add_argument("PLOT_TYPE", choices=["balanced", "unbalanced", "bert"])

    parser.add_argument(
        "--ln", type=str, required=True, help="Path to measurements for Locally Nameless.",
    )
    parser.add_argument(
        "--ours", type=str, required=True, help="Path to measurements for Ours."
    )
    parser.add_argument(
        "--db", type=str, required=True, help="Path to measurements for de Bruijn."
    )
    parser.add_argument(
        "--struct", type=str, required=True, help="Path to measurements for Structural."
    )

    # Enable LaTeX, and set the default font size.
    matplotlib.rcParams["text.usetex"] = True
    matplotlib.rcParams["font.size"] = 13

    args = parser.parse_args()

    results = {
        "LN": args.ln,
        "Ours": args.ours,
        "DeBruijn": args.db,
        "Structural": args.struct,
    }

    if args.PLOT_TYPE == "balanced":
        make_plot(
            filename="benchmark-balanced.pdf",
            descr="balanced expressions",
            bert_plot=False,
            results=results,
        )
    elif args.PLOT_TYPE == "unbalanced":
        make_plot(
            filename="benchmark-unbalanced.pdf",
            descr="unbalanced expressions",
            bert_plot=False,
            results=results,
        )
    elif args.PLOT_TYPE == "bert":
        make_plot(
            filename="benchmark-bert.pdf",
            descr="BERT",
            bert_plot=True,
            results=results,
        )
    else:
        raise ValueError(f"Unrecognized plot type {args.PLOT_TYPE}.")


if __name__ == "__main__":
    main()
