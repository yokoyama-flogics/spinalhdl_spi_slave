# Simple SPI Peripheral (COPI only) written by SpinalHDL

- Learning how to design clock domain crossing receiver.

> - COPI: Controller Output Peripheral Input

## Preparation

1. Follow [this site](https://github.com/SpinalHDL/SpinalHDL) and install required software SpinalHDL.

## Build

```bash
$ sbt "runMain flogics.lib.spi.SpiSlaveSim"
```

or

```bash
$ sbt "runMain flogics.lib.spi.SpiSlaveVerilog"
```

## Blog in Japanese

- [Added SPI Peripheral functionality to Murax (free RISC-V SoC)](https://flogics.com/wp/ja/2020/04/added-spi-slave-to-vexriscv-soc-murax/)
