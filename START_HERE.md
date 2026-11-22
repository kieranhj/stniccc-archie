# stniccc-archie: start here

A port of Oxygene's STNICCC-2000 to the Acorn Archimedes that turned into the demo "Back By
Popular Demand" (Bitshifters, NOVA 2020). Code by kieran, music/sequencing/gfx Rhino, logos Spiny.

| | |
|---|---|
| GitHub | https://github.com/kieranhj/stniccc-archie (public) |
| Upstream | STNICCC-2000 by Oxygene (Atari ST); scene data in `data\stniccc\` |
| Local path | D:\archie\mine\stniccc-archie (moved from OneDrive\Archie\Repos\stniccc-archie on 2026-11-22) |
| Branch | rocket-sync (10 commits ahead of master); others: master, events-system, exomizer-bytes, exomizer-jump, exomizer-port, plot-frame-by-index, vanilla-parser, wip/2026-11-tidy |
| Last commit | 2020-09-24: Default to sync'd with the editor. |

## What's in here
- `stniccc.asm`: top-level; `parser.asm`, `plot.asm`, `plot_unroll.asm`: the polygon stream player;
  `rocket.asm`, `events.asm`, `image.asm`, `palette.asm`, `lz4-decode.asm`.
- `data\`: scene stream, music, graphics, readme; `folder\`: files copied into the app.
- `make.bat`: the build; `make-assets.bat`: PNG to Archimedes conversion plus lz4 into `build\`.
- `!Demand\`, `!DemandS\`: built app folders (ignored).

## Build and run
- `make`: runs `make.bat` (works, 2026-11-22, with a caveat): assembles `build\stniccc.bin`, builds
  `!DemandS` and copies it to `..\..\Arculator_V2.0_Windows\hostfs\!DemandS`.
  Caveat: `data\stniccc\scene1.bin` is currently deleted in the working tree, so the copy of
  `Scene1,ffd` fails and `!DemandS` has no scene data.
- `make-assets.bat` (not wrapped) runs `python bin\png2arc.py`, which is Python 2 code; the
  converted `build\*.bin/.lz4` files already exist.
- Run: in Arculator 2.0, double-click `!DemandS` in the HostFS filer.

## Local changes
- Uncommitted now: `data/stniccc/scene1.bin` deleted (639,976 bytes).
- wip/2026-11-tidy (pushed 2026-11-21): snapshot of that deletion on top of rocket-sync.
- rocket-sync: GNU Rocket sync tracks (exported data) and "default to sync'd with the editor".
- No stashes.

## Status and next steps
- Released 2020-06-20 at NOVA; rocket-sync (2020-07 to 2020-09) is post-release work syncing the
  demo to a Rocket editor. No TODO or plan notes found.
- Decide whether the `scene1.bin` deletion is intended (`git restore data/stniccc/scene1.bin` would undo it).

## See also
- README.md (credits, links to the Bitshifters prod page, Pouet, YouTube), D:\archie\mine\emoon-rocket.
