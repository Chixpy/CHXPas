## CHXSDL3Engine

- `cCHXSDL3Engine` saves automatically config file if readed one and autoinit
  is True on creation. It auto-saves config too if `Config.DefaultFileName`
  is set in any other case (of course `Config.SaveToFile` can be used manually).
- Added keys [F10] and [F12] to change frame rate.
- [F10] toggles show frame info inside the window instead change window title.
- `cCHXSDL3Renderer` added `DebugText` and `DebugTextF`.
