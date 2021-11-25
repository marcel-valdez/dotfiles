#!/usr/bin/env python3.8

"""Given a stdin input of data it will calculate the mean of the
streaming lines and print any value that is 3 standard deviations
outside of the mean along with a timetamp."""

from typing import List, Iterable, Callable
from threading import Thread, Lock

import sys
import time
import argparse
import random
import signal
import matplotlib.pyplot as plt
import matplotlib.animation as animate


class Sample:
    """Single sample point for the latency analysis."""

    def __init__(self, value: int, timestamp_ms: int):
        self.__value = value
        self.__timestamp_ms = timestamp_ms

    @property
    def value(self) -> int:
        return self.__value

    @property
    def timestamp_ms(self) -> int:
        return self.__timestamp_ms

    @property
    def timestamp_sec(self) -> int:
        return self.__timestamp_ms / 1000


def sample(latency_ms: int) -> Sample:
    return Sample(latency_ms, time.time() * 1000)


class TimeSeriesPlotter:
    def __init__(
            self, plot_title: str, axes: plt.Axes, figure: plt.Figure,
            live_updates: bool
    ):
        self.plot_title = plot_title
        self.axes = axes
        self.figure = figure
        self.start_sec = None
        self.times = []
        self.values = []
        self.live_updates = live_updates
        if live_updates:
            self.axes.set(
                xlabel="time (sec)",
                ylabel="latency (ms)",
                title=self.plot_title
            )
            self.axes.grid()
            self.old_line = self.axes.plot(
                self.times, self.values, color='blue'
            )[0]
            self.animation = animate.FuncAnimation(
                figure, self.plot_animation_frame, interval=250,
            )

    def plot_animation_frame(self, *_) -> None:
        self.old_line.remove()
        self.old_line = self.axes.plot(
            self.times, self.values, color='blue'
        )[0]

    def add(self, sample_pt: Sample) -> None:
        if self.start_sec is None:
            self.start_sec = sample_pt.timestamp_sec
        relative_timestamp_sec = sample_pt.timestamp_sec - self.start_sec
        self.values.append(sample_pt.value)
        self.times.append(relative_timestamp_sec)

    def gen_figure(self, file_path: str) -> None:
        if not self.live_updates:
            self.axes.set(
                xlabel="time (sec)", ylabel="latency (ms)",
                title=self.plot_title
            )
            self.axes.grid()
            self.axes.plot(self.times, self.values, color='blue')
        self.figure.savefig(file_path)


class Calculator:
    def __init__(self, buckets_fn: Callable[[float], float]):
        self.sample_points: List[float] = []
        self.count = 0
        self.sum = 0
        self.max = -1
        self.min = 999999999
        self.buckets = {}
        self.buckets_fn = buckets_fn

    def new_entry(self, sample_pt: Sample) -> None:
        self.count += 1
        self.sum += sample_pt.value
        if sample_pt.value > self.max:
            self.max = sample_pt.value
        elif sample_pt.value < self.min:
            self.min = sample_pt.value

        bucket_key = self.buckets_fn(sample_pt.value)
        self.buckets[bucket_key] = self.buckets.get(bucket_key, 0) + 1

    def mean(self) -> float:
        if self.count == 0:
            return 0
        return self.sum / self.count

    def buckets_report(self) -> str:
        report = ""
        for bucket_name in sorted(self.buckets.keys()):
            bucket_count = self.buckets[bucket_name]
            bucket_percent = (float(bucket_count) / self.count) * 100
            report += f"{bucket_name}:{bucket_count} ({bucket_percent:.1f}%) "

        return report

    def sample_size(self) -> int:
        return self.count

    def full_report(self) -> str:
        return (
            f"mean:{int(self.mean())} size:{self.count} min:{self.min}"
            f" max:{self.max} {self.buckets_report()}"
        )


def latency_buckets(value: float) -> str:
    bucket = None
    if value <= 100:
        bucket = "0-100"
    elif value <= 150:
        bucket = "101-150"
    elif value <= 250:
        bucket = "151-250"
    elif value <= 500:
        bucket = "251-500"
    elif value <= 1000:
        bucket = "501-1000"
    elif value <= 5000:
        bucket = "1001-5000"
    else:
        bucket = "5000+"
    return bucket


def stream_stdin(stdin) -> Iterable[float]:
    line = None
    while line != '':
        line = stdin.readline()
        if line:
            yield sample(float(line))


class Unbuffered(object):
    def __init__(self, stream):
        self.stream = stream

    def write(self, data):
        self.stream.write(data)
        self.stream.flush()

    def writelines(self, datas):
        self.stream.writelines(datas)
        self.stream.flush()

    def __getattr__(self, attr):
        return getattr(self.stream, attr)


def main(plot_file: str, plot_title: str, live_updates: bool):
    calculator = Calculator(latency_buckets)
    samples = stream_stdin(Unbuffered(sys.stdin))
    stdout = Unbuffered(sys.stdout)
    figure, axes = plt.subplots()
    plotter = TimeSeriesPlotter(plot_title, axes, figure, live_updates)
    lock = Lock()

    def exit_gracefully(*_args):
        # Only allow this method to be entered once by never releasing the lock
        if lock.acquire(blocking=False):
            print(calculator.full_report(), file=stdout)
            plotter.gen_figure(plot_file)
            if live_updates:
                plot_animation.join()
            else:
                plt.show()

    signal.signal(signal.SIGTERM, exit_gracefully)

    if live_updates:
        plot_animation = Thread(target=plt.show)
        plot_animation.start()

    try:
        for sample_pt in samples:
            calculator.new_entry(sample_pt)
            plotter.add(sample_pt)
            print(calculator.full_report(), end="\r", file=stdout)
    finally:
        exit_gracefully()


def gen_random_plot_file() -> str:
    return f"plot_{random.randint(0, 999999)}.png"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        "Latency Calculator",
        description="Measures latency and produces a report and plot with the data."
    )
    parser.add_argument(
        "--plot_file", "-f",
        help="File onto which to save an image of the plot",
        type=str,
        required=False
    )
    parser.add_argument(
        "--plot_title", "-t",
        help="Title for the generated plot",
        type=str,
        required=False
    )
    parser.add_argument(
        "--live", "-l",
        help="Whether to show a plot with live updates or not",
        action="store_true",
        required=False,
        default=False
    )
    args = parser.parse_args()
    main(
        args.plot_file or gen_random_plot_file(),
        args.plot_title or "Latency over time",
        args.live
    )
