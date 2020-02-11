module tagion.utils.Term;

import std.format;

enum {
    BLACK   = Color.Black.code,
    RED     = Color.Red.code,
    GREEN   = Color.Green.code,
    YELLOW  = Color.Yellow.code,
    BLUE    = Color.Blue.code,
    MAGENTA = Color.Magenta.code,
    CYAN    = Color.Cyan.code,
    WHITE   = Color.White.code,

    BACKGOUND_BLACK   = Color.Black.code(true),
    BACKGOUND_RED     = Color.Red.code(true),
    BACKGOUND_GREEN   = Color.Green.code(true),
    BACKGOUND_YELLOW  = Color.Yellow.code(true),
    BACKGOUND_BLUE    = Color.Blue.code(true),
    BACKGOUND_MAGENTA = Color.Magenta.code(true),
    BACKGOUND_CYAN    = Color.Cyan.code(true),
    BACKGOUND_WHITE   = Color.White.code(true),
    BACKGOUND_RESET   = Color.Reset.code(true),

    RESET   = Color.Reset.code,
}

enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Reset,
}


string code(
    immutable Color c,
    immutable bool background=false,
    immutable bool bright=false) {
    if (c is Color.Reset) {
        return "\u001b[%s%";
    }
    else {
        immutable background_color=(background)?"4":"3";
        immutable bright_color=(bright)?"m":";1m";
        return format("\u001b[%s%d%s", background_color, c, bright_color);
    }
    assert(0);
}

enum Cursor : string {
    Up = "A",   /// Moves cursor up by n
    Down = "B", /// Moves cursor down by n
    Right ="C", /// Moves cursor right by n
    Left = "D", /// Moves cursor left by n
    NextLine = "E",  /// Moves cursor to beginning of line n lines down
    PrevLine = "F",  /// Moves cursor to beginning of line n lines down
    SetColumn = "G", /// Moves cursor to column n
    ClearScreen = "J", // clears the screen
    ClearLine = "K"     // clears the current line
}

string code(immutable Cursor c, immutable uint n=1) {
    return format("\u001b[%d", n, c);
}


enum Mode {
    None = 0,       /// All attributes off
    Bold = 1,       /// Bold on
    Underscore = 4, /// Underscore (on monochrome display adapter only)
    Blink = 5,	    /// Blink on
    Reverse =7,     /// Reverse video on
    Concealed = 8,  /// Concealed on
}

string code(immutable Mode m) {
    return format("\u001b[%dm", m);
}

string setCursor(immutable uint row, immutable uint column) {
    return format("\u001b[%d;%dH", row, column);
}

enum saveCursorPos   ="\u001b[s"; /// Saves the current cursor position
enum restoreCursorPos="\u001b[u"; /// Restores the cursor to the last saved position
