---
title: 2015 ANU CS Honours/Masters/Student projects
alias: ["./2015-02-04-2015-anu-cs-honours-projects.html"]
tags: contributing
---

This is a list of possible ANU Computer Science Honours projects for
2015, supervised by Henry Gardner and me. Send
[Henry](mailto:henry.gardner@anu.edu.au) or
[myself](mailto:ben.swift@anu.edu.au) an email if you’re interested.

If you like the style of any of them but would like to take them in a
different direction, then let me know as well—I’m sure we can find a fun
project which works for everyone.

# Live steering of parallel physics particle simulation codes

This project will look at run-time load balancing and optimisation of
scientific simulations running on parallel computing architectures.
[Extempore](http://extempore.moso.com.au) is a programming environment
for live programming, which combines just-in-time (JIT) interactive
compilation with high-performance native code generation.

Extempore has already been used for [interactive steering of
particle-in-cell (PIC) plasma physics simulation
codes](https://vimeo.com/99891379). This project provides an opportunity
to continue this work, working with both live programmers and physicists
at the ANU and overseas to harness and profile plasma physics PIC codes
in this live programming workflow.

This project would involve:

-   prototyping simulations on parallel computers in the Research School
    of Computer Science under real time control of Extempore
-   implementing library/tooling support in Extempore to provide a
    programmer with appropriate feedback and control of a simulation
-   evaluating the opportunities and challenges of live steering in this
    context (e.g. what safeguards must the tooling provide, what
    timescales and algorithmic “granularity” provides the most fruitful
    domain for live steering)

\*Desirable skills/interests:\* Strong programming skills (especially
C); scientific and numerical computation; parallel programming;
high-performance computing and optimisation.

# Use of live-coding to revitalise a legacy scientific simulation code

This project will apply a modern live-coding tools & techniques to a
common problem in the scientific simulation community—how to “breathe
life” into legacy simulation codes. Often these codes are used for many
years (or decades!) after they were first developed. They are often used
by experimental scientists to model experiments which are quite
different from those existing when the simulations were first developed.
In the absence of the original programmers, there is a need to bring
these simulations back to life and to return them to the
experimentalists with modern user interfaces and functionality.

The recent advent of live-coding provides a new opportunity to address
this problem. Live coding environments such as
[Extempore](http://extempore.moso.com.au) can be used to [harness and
interactively steer legacy simulation
codes](https://vimeo.com/99891379). It is possible to pull out important
function calls, to reparameterise them, and to visualise their effects,
in real time and in the presence of a scientific user.

This project will investigate the usefulness of this approach for a
concrete simulation from the ANU Research School of Physical Sciences
and Engineering.

This project would involve:

-   prototyping a particle-in-cell physics simulation of an experiment
    (a plasma rocket)
-   live-steering this simulation in the presence of scientific users
-   developing a user interface and enhanced functionality for this
    simulation
-   reflecting on the software engineering context and future
    opportunities afforded by this live coding approach

\*Desirable skills/interests:\* Strong programming skills (especially
C); scientific and numerical computation; parallel programming;
human-computer interaction; software engineering.

# Visual Live Coding as Collaborative Performance

In [live coding](http://vimeo.com/videos/benswift) a programmer/artist
writes a program to generate audiovisual material (often music) in
real-time, in front of an audience. During a performance, the source
code is projected onto a screen as it is edited for the audience’s
benefit.

This display of code for an audience is a key part of the practice of
livecoding, but many audience members lack the required background
knowledge (either musical, programming or both) to fully comprehend the
relationship between the code on the screen and the musical material
they are hearing. Some techniques have been
[proposed](http://eprints.qut.edu.au/61525/1/liveannotations.pdf) for
visual ‘annotations’ to the code which may assist the audience in their
comprehension. Additionally, non-textual visualisation techniques from
VJing and computational arts (e.g. beat-driven 3D visuals in OpenGL) may
be combined with source code to provide visual feedback which balances
didactic and aesthetic considerations. A 2014 ANU Honours project[^1]
investigated various techniques for presenting visual feedback to an
audience during a live coding performance in
[Extempore](http://extempore.moso.com.au), and evaluated their
effectiveness through user studies of livecoding audiences.

This present project will look at ways of developing a visual live
coding system that is linked (over the network) to an music/audio
livecoding system. This will provide a platform for two collaborating
artists (one with live audio, one with live visuals) to collaborate in
real-time. The project will investigate effective ways of designing and
supporting such a collaborative artistic endeavour.

**Desirable skills/interests:** Strong programming skills;
human-computer interaction; computational arts & visualisation; software
engineering.

# Scientific Workflows Including Dynamic workflow Engines

Scientific workflow engines provide non-specialist programmers with a
means to prepare, submit and analyse complex scientific simulations on
high-performance distributed computing platforms. At present, most of
these workflow engines are static—they do not allow for interaction with
a running simulation.

Dynamic Workflow Engines (DWEs) could provide support for modifying the
evolution of specific tasks in a high-performance scientific simulation
(e.g. swapping out low-level algorithms and “hot loops” in
computation)[^2]. Such a level of adaptivity would allow for
computational steering possibilities, but may exacerbate problems of
deciding when and how to safely and securely interfere with a running
simulation.

This project will develop a specific DWE case study involving a
live-coding harness of a scientific simulation. The simulation domain
will be Particle-In-Cell plasma physics and the live-coding harness will
use the [Extempore](http://extempore.moso.com.au) environment.

**Desirable skills/interests:** Strong programming skills (especially
C); scientific and numerical computation (helpful but not essential);
software engineering; scientific workflows.

# Live code combat: human-in-the-loop simulation and agent-based models<span class="tag" data-tag-name="noexport"></span>

Wargames-style simulation is an important tool in understanding the
behaviour of complex multi-agent environments, and code-based “combat”
simulation (where multiple programs do battle, subject to rules and
resource constraints) has [pedagogical applications as
well](https://codecombat.com/).

This project provides an opportunity to investigate how “live
programming” tools and techniques (specifically the [Extempore
programming environment](http://extempore.moso.com.au)) may be used for
human in-the-loop (HIL) programmer interaction in wargames simulation.
For example, a simulation may be started with two different “armies”
(groups) of agents, with basic initial conditions and simple agent
behaviour. As the simulation unfolds, programmer(s) may re-compile and
hot-swap the code which determines the agent’s behaviour, changing the
course of the simulation. Other programmers may then intervene, in
response to the changing agent behaviour and nature of the simulation.

In this way, simple agent-based models can be used to respond to changes
in battlefield conditions through human-in-the-loop intervention. This
project will involve building tools in Extempore for live HIL
agent-based simulation, and evaluate their effectiveness against other
(non-interactive) simulation approaches.

**Desirable skills/interests:** Strong programming skills (especially
C); human-computer interaction; parallel programming; agent-based
models; human-in-the-loop simulation.

[^1]: Arrian Purcell, Honours thesis, ANU 2014

[^2]: Marta Maoso, Jonas Dias, Kary A. C. S. Ocana, Eduardo Ogasawara,
    Flavio Costa, Felipe Horta, Vítor Silva, and Daniel de Oliveira.
    “Dynamic steering of HPC scientific workflows: A survey”. In: Future
    Generation Computer Systems (2014)
