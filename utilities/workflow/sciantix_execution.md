# SCIANTIX execution workflow

This is a set of four hand-maintained diagrams, each mapping onto one
source-level scope, from the outermost driver loop down to the OpenCalphad
(OC) coupling. Each diagram ends where the next one begins, so read them in
order:

1. **Program flow** -- the `MainSCIANTIX.C` driver loop and its input files.
2. **`SCIANTIX()` call sequence** -- `Sciantix.C` and the five `Simulation::set*`
   calls made by `Simulation::initialize()`, followed by `execute()`,
   `update()`, `output()`.
3. **`Simulation::execute()` physics sequence** -- the ordered list of physics
   calls in `src/classes/Simulation.C`, grouped the way the file's own
   comments group them (burn-up/microstructure, fuel chemistry, fission
   product behaviour, gap behaviour).
4. **OC coupling** -- what `Simulation::SetPhaseDiagram()`
   (`src/operations/SetPhaseDiagram.C`) actually does when it hands off to
   OpenCalphad via `src/coupling/OCUtilsCoupling.C`.

Diagram 4 is deliberately still an abstraction, not a line-by-line translation
of `SetPhaseDiagram.C`: the real function also handles per-location
equilibrium-staleness caching (temperature/pressure/composition tolerances)
and retries several OpenCalphad solver modes before giving up. Read
`SetPhaseDiagram.C` and `OCUtilsCoupling.C` directly if you need that level of
detail -- the diagram only shows the decision points that change the control
flow (skip vs. solve, cache-hit vs. cache-miss).

Two things worth calling out that are easy to miss from the code alone:

- **`-DCOUPLING_TU` removes physics, not just changes behaviour.** When
  SCIANTIX is compiled for TRANSURANUS coupling, `Simulation::execute()`
  compiles out `Burnup()`, `EffectiveBurnup()`, `Densification()`,
  `GapPartialPressure()`, and `UO2Thermochemistry()` entirely (`#if
  !defined(COUPLING_TU)` in `Simulation.C`) -- burnup and gap pressure/oxygen
  potential are supplied by the host code instead. Diagram 3 marks these with
  a dashed fill.
- **There is no separate "solver declaration" phase.** Every physics call in
  `execute()` declares its own local `Model`, resolves it through the
  `Simulation`-owned `Solver` instance, and discards it -- e.g.
  `Simulation::Burnup()` and `Simulation::GrainGrowth()` each build their own
  `Model model_;` and call `solver.Integrator(...)` /
  `solver.QuarticEquation(...)` inline. This repeats independently in each of
  the ~20 calls in diagram 3; it is not a pipeline stage you can point to.

## 1. Program flow -- `MainSCIANTIX.C`

```mermaid
---
title: Program flow -- MainSCIANTIX.C
---
flowchart TB
    subgraph INPUTS["input files"]
        direction TB
        I_HIST>"HISTORY<br/>(t, T, $\dot{F}$, σ_h, p, O/M)"]
        I_OPT>"MODELS' OPTION"]
        I_IC>"INITIAL CONDITIONS"]
        I_SF>"SCALING FACTORS"]
        I_TCOPT>"THERMOCHEMISTRY OPTION"]
        I_TCDB>"THERMOCHEMISTRY DATABASE"]
        I_HIST ~~~ I_OPT ~~~ I_IC ~~~ I_SF ~~~ I_TCOPT ~~~ I_TCDB
    end

    subgraph MAIN["MainSCIANTIX.C"]
        direction TB
        M_BEGIN(["BEGIN"])
        M_INPUT[/"INPUT<br/>(InputReading)"/]
        M_INIT["INITIALIZATION"]
        M_LOOP{"TIME LOOP<br/>t ≤ t_end ?"}
        M_INTERP["INPUT INTERPOLATION"]
        M_CALL["SCIANTIX()<br/>see: call-sequence diagram"]
        M_STEP["TIME-STEP CALCULATION"]
        M_END(["END"])

        M_BEGIN --> M_INPUT --> M_INIT --> M_LOOP
        M_LOOP -- yes --> M_INTERP --> M_CALL --> M_STEP --> M_LOOP
        M_LOOP -- no --> M_END
    end

    I_HIST --> M_INPUT
    I_OPT --> M_INPUT
    I_IC --> M_INPUT
    I_SF --> M_INPUT
    I_TCOPT --> M_INPUT
    I_TCDB --> M_INPUT

    classDef loopbox fill:#E6E1EA,stroke:#5b5766,stroke-width:1.5px,color:#28252e;
    classDef input fill:#F0D77B,stroke:#8a6d1f,stroke-width:1.5px,color:#3a2e0d;
    classDef highlight fill:#7A1F1F,stroke:#3a0d0d,stroke-width:2px,color:#ffffff;

    class M_BEGIN,M_END,M_INPUT,M_INIT,M_LOOP,M_INTERP,M_STEP loopbox;
    class M_CALL highlight;
    class I_HIST,I_OPT,I_IC,I_SF,I_TCOPT,I_TCDB input;
```

![program_flow.png](images/program_flow.png)

## 2. `SCIANTIX()` call sequence -- `Sciantix.C` + `Simulation::initialize/execute/update/output`

```mermaid
---
title: SCIANTIX() call sequence -- Sciantix.C + Simulation::initialize/execute/update/output
---
flowchart TB
    C_BEGIN(["BEGIN"])
    setVariables["SET VARIABLES"]
    setFissionProducts["SET FISSION PRODUCTS"]
    setMatrix["SET FUEL MATRIX"]
    setSystem["SET SYSTEM"]
    setGPVariables["SET GP VARIABLES"]
    execute["EXECUTE PHYSICS SEQUENCE<br/>see: execute-sequence diagram"]
    update["UPDATE VARIABLES"]
    output[/"OUTPUT"/]
    C_END(["END"])

    C_BEGIN --> setVariables --> setFissionProducts --> setMatrix --> setSystem --> setGPVariables --> execute --> update --> output --> C_END

    setFissionProducts_note["gas: Xe, Kr<br/>volatile: Cs<br/>metallic: Mo, Pd, Ru, Rh, Tc<br/>ceramic: Ba"]
    setFissionProducts --> setFissionProducts_note
    setMatrix_note["UO2, MOX"]
    setMatrix --> setMatrix_note
    setSystem_note["FPs in fuel matrix"]
    setSystem --> setSystem_note
    setGPVariables_note["only active for iFissionProductDiffusivity == 90<br/>(GP-regression Xe diffusivity)"]
    setGPVariables --> setGPVariables_note

    classDef terminal fill:#E8C4BE,stroke:#7A1F1F,stroke-width:1.5px,color:#3a0d0d;
    classDef process fill:#E8C4BE,stroke:#7A1F1F,stroke-width:1.5px,color:#3a0d0d;
    classDef note fill:#F6E9D9,stroke:#7A1F1F,stroke-width:1px,color:#3a0d0d;
    classDef highlight fill:#7A1F1F,stroke:#3a0d0d,stroke-width:2.5px,color:#ffffff;

    class C_BEGIN,C_END terminal;
    class setVariables,setFissionProducts,setMatrix,setSystem,setGPVariables,update,output process;
    class execute highlight;
    class setFissionProducts_note,setMatrix_note,setSystem_note,setGPVariables_note note;
```

![call_sequence.png](images/call_sequence.png)

## 3. `Simulation::execute()` physics sequence -- `src/classes/Simulation.C`

```mermaid
---
title: Simulation::execute() physics sequence -- src/classes/Simulation.C
---
flowchart TB
    subgraph BURN["Burn-up & microstructure"]
        direction TB
        Burnup["BURNUP"] --> EffectiveBurnup["EFFECTIVE BURNUP"] --> Densification["DENSIFICATION"] --> HighBurnupStructureFormation["HBS FORMATION"] --> HighBurnupStructurePorosity["HBS POROSITY"] --> GrainGrowth["GRAIN GROWTH"]
    end

    subgraph CHEM["Fuel matrix chemistry"]
        direction TB
        ChromiumSolubility["CHROMIUM SOLUBILITY"] --> GapPartialPressure["GAP PARTIAL PRESSURE"] --> UO2Thermochemistry["FUEL THERMOCHEMISTRY"] --> StoichiometryDeviation["STOICHIOMETRY DEVIATION"]
    end

    subgraph FP["Fission product behaviour"]
        direction TB
        GrainBoundarySweeping["GRAIN-BOUNDARY SWEEPING"] --> FissionProductProduction["FP PRODUCTION"] --> FissionProductDecay["FP DECAY"] --> IntraGranularBubbleBehavior["INTRAGRANULAR BUBBLE BEHAVIOUR"] --> IntragranularDiffusion["INTRAGRANULAR DIFFUSION"] --> SetPhaseDiagram["SET PHASE DIAGRAM<br/>see: OC-coupling diagram"] --> GrainBoundaryMicroCracking["GRAIN-BOUNDARY MICROCRACKING"] --> GrainBoundaryVenting["GRAIN-BOUNDARY VENTING"] --> InterGranularBubbleBehavior["INTERGRANULAR BUBBLE BEHAVIOUR"] --> FissionProductRelease["FP RELEASE"]
    end

    subgraph GAP["Gap behaviour"]
        direction TB
        JOGFormation["JOG FORMATION"]
    end

    GrainGrowth --> ChromiumSolubility
    StoichiometryDeviation --> GrainBoundarySweeping
    FissionProductRelease --> JOGFormation

    TUNote["dashed fill = compiled out when built with -DCOUPLING_TU (TRANSURANUS coupling):<br/>BURNUP, EFFECTIVE BURNUP, DENSIFICATION, GAP PARTIAL PRESSURE, FUEL THERMOCHEMISTRY"]
    ModelNote["every step above declares its own local Model and resolves it<br/>through the shared Solver instance -- see Model.h / Solver.h"]

    classDef note fill:#F6E9D9,stroke:#7A1F1F,stroke-width:1px,color:#3a0d0d;
    classDef simProcess fill:#D9D9B8,stroke:#6B6B3A,stroke-width:1.5px,color:#2b2b16;
    classDef simCouplingSkip fill:#D9D9B8,stroke:#6B6B3A,stroke-width:1.5px,stroke-dasharray: 5 3,color:#2b2b16;
    classDef simHighlight fill:#D9D9B8,stroke:#8A4A2A,stroke-width:2.5px,color:#2b2b16;

    class Burnup,EffectiveBurnup,Densification,GapPartialPressure,UO2Thermochemistry simCouplingSkip;
    class HighBurnupStructureFormation,HighBurnupStructurePorosity,GrainGrowth,ChromiumSolubility,StoichiometryDeviation,GrainBoundarySweeping,FissionProductProduction,FissionProductDecay,IntraGranularBubbleBehavior,IntragranularDiffusion,GrainBoundaryMicroCracking,GrainBoundaryVenting,InterGranularBubbleBehavior,FissionProductRelease,JOGFormation simProcess;
    class SetPhaseDiagram simHighlight;
    class TUNote,ModelNote note;
```

![execute_sequence.png](images/execute_sequence.png)

## 4. OC coupling -- `src/operations/SetPhaseDiagram.C` + `src/coupling/OCUtilsCoupling.C`

```mermaid
---
title: OC coupling (thermochemistry) -- src/operations/SetPhaseDiagram.C + src/coupling/OCUtilsCoupling.C
---
flowchart TB
    O1{"run full OpenCalphad?<br/>(iThermochimica mode, COUPLING_TU)"}
    Oskip["CARRY FP INVENTORY<br/>WITHOUT THERMOCHEMISTRY"]
    O2["FOR EACH LOCATION<br/>(matrix, grain boundary)"]
    O3["BUILD INPUT COMPONENTS<br/>(elements + composition)"]
    O4{"cached equilibrium<br/>still fresh?"}
    O5["REUSE CACHED<br/>EQUILIBRIUM"]
    O6["SOLVE EQUILIBRIUM<br/>(retry solver modes)"]
    O7["WRITE THERMOCHEMISTRY<br/>VARIABLES"]
    O8["UPDATE MATRIX /<br/>GRAIN-BOUNDARY FP INVENTORY"]

    O1 -- no --> Oskip
    O1 -- yes --> O2 --> O3 --> O4
    O4 -- yes --> O5 --> O7
    O4 -- no --> O6 --> O7
    O7 --> O8

    classDef simOc fill:#F3D9C4,stroke:#8A4A2A,stroke-width:1.5px,color:#3a1f0d;
    class O1,Oskip,O2,O3,O4,O5,O6,O7,O8 simOc;
```

![oc_coupling.png](images/oc_coupling.png)

## Maintaining these diagrams

These files are plain, hand-maintained Mermaid diagrams -- there is no script
that regenerates them from the C++ source, so when `Sciantix.C`,
`Simulation.C`, or `SetPhaseDiagram.C` change in a way that affects control
flow, edit the corresponding `.mmd` file (and the matching fenced block
above) directly.

Each `.mmd` file renders standalone. To regenerate a PNG after editing one:

```bash
npx -p @mermaid-js/mermaid-cli mmdc \
  -i program_flow.mmd -o images/program_flow.png \
  -p puppeteer-config.json -b white -w 1800
```

(swap in `call_sequence`, `execute_sequence`, or `oc_coupling` as needed).
