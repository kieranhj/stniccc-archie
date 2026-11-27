# stniccc-archie: start here

A port of Oxygene's STNICCC-2000 to the Acorn Archimedes that turned into the demo "Back By
Popular Demand" (Bitshifters, NOVA 2020). Code by kieran, music/sequencing/gfx Rhino, logos Spiny.

| | |
|---|---|
| GitHub | https://github.com/bitshifters/stniccc-archie (public; transferred from kieranhj/stniccc-archie on 2026-11-27, old URL redirects) |
| Upstream | STNICCC-2000 by Oxygene (Atari ST); scene data in `data\stniccc\` |
| Local path | D:\bitshifters\stniccc-archie (moved from D:\archie\mine on 2026-11-27; before that OneDrive\Archie\Repos) |
| Branch | rocket-sync (10 commits ahead of master); others: master, events-system, exomizer-bytes, exomizer-jump, exomizer-port, plot-frame-by-index, vanilla-parser, wip/2026-11-tidy |
| Last commit | 2020-09-24: Default to sync'd with the editor. |

## What's in here
- `stniccc.asm`: top-level; `parser.asm`, `plot.asm`, `plot_unroll.asm`: the polygon stream player;
  `rocket.asm`, `events.asm`, `image.asm`, `palette.asm`, `lz4-decode.asm`.
- `data\`: scene stream, music, graphics, readme; `folder\`: files copied into the app.
- `make.bat`: the build; `make-assets.bat`: PNG to Archimedes conversion plus lz4 into `build\`.
- `!Demand\`, `!DemandS\`: built app folders (ignored).

## Build and run
- `make`: runs `make.bat`: assembles `build\stniccc.bin`, builds `!DemandS` and copies it to
  `..\arculator\hostfs\Builds\!DemandS` (checked 2026-11-27 on rocket-sync, including `Scene1,ffd`).
- `make-assets.bat` (not wrapped) runs `python bin\png2arc.py` (Python 3). `bin\png2arc.py` and
  `bin\arc.py` were ported to Python 3 on 2026-11-22, and `make-assets.bat` regenerates all 74
  `build\*.bin/.pal/.lz4` files byte-identical to the 2020 ones. `bin\encoder.py` (the scene
  encoder, not part of the build) is still Python 2.
- Run: in Arculator 2.0, double-click `!DemandS` in the HostFS filer.

## Local changes
- Working tree clean (the earlier `scene1.bin` deletion is no longer present).
- wip/2026-11-tidy (pushed 2026-11-21): snapshot of that deletion on top of rocket-sync.
- rocket-sync: GNU Rocket sync tracks (exported data) and "default to sync'd with the editor".
- No stashes.

## Status and next steps
- Released 2020-06-20 at NOVA; rocket-sync (2020-07 to 2020-09) is post-release work syncing the
  demo to a Rocket editor. No TODO or plan notes found.

## See also
- README.md (credits, links to the Bitshifters prod page, Pouet, YouTube), D:\archie\mine\emoon-rocket.
