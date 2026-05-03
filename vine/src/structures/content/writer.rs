use crate::structures::content::Length;

pub struct Writer<'a> {
  pub max_width: Length,
  state: State,
  indent: u16,
  output: &'a mut dyn Output,
}

enum State {
  Text,
  Space,
  Line,
  Blank,
}

impl<'a> Writer<'a> {
  pub fn new(max_width: Length, output: &'a mut dyn Output) -> Self {
    Writer { max_width, state: State::Line, indent: 0, output }
  }

  /// Require that a space exist at the current position. This method is
  /// idempotent, and is a no-op when called immediately before or after `line`
  /// or `blank`. It can thus be called without worrying that multiple spaces
  /// will be emitted, or that extraneous leading or trailing spaces will be
  /// emitted.
  pub fn space(&mut self) {
    match self.state {
      State::Text | State::Space => self.state = State::Space,
      State::Line | State::Blank => {}
    }
  }

  /// Require that a newline exist at the current position. This method is
  /// idempotent, and is a no-op when called immediately before or after
  /// `blank`. It can thus be called without worrying that multiple newlines
  /// will be emitted, when a blank line wasn't desired.
  pub fn line(&mut self) {
    match self.state {
      State::Text | State::Space => {
        self.output.line();
        self.state = State::Line;
      }
      State::Line | State::Blank => {}
    }
  }

  /// Require that a blank line exist at the current position. This method is
  /// idempotent. It can thus be called without worrying that multiple blank
  /// lines will be emitted.
  pub fn blank(&mut self) {
    match self.state {
      State::Text | State::Space => {
        self.output.line();
        self.output.line();
        self.state = State::Blank;
      }
      State::Line => {
        self.output.line();
        self.state = State::Blank;
      }
      State::Blank => {}
    }
  }

  /// The current line width; this is the `max_width` minus the current indent
  /// width.
  pub fn width(&self) -> Length {
    self.max_width - Length(self.indent * 2)
  }

  pub fn indent<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
    self.indent += 1;
    self.line();
    let output = f(self);
    self.line();
    self.indent -= 1;
    output
  }

  pub fn indent_if<T>(&mut self, indent: bool, f: impl FnOnce(&mut Self) -> T) -> T {
    self.indent += indent as u16;
    self.line();
    let output = f(self);
    self.line();
    self.indent -= indent as u16;
    output
  }

  /// Flushes any pending whitespace before printing non-whitespace characters.
  fn pre_write(&mut self) {
    match self.state {
      State::Text => {}
      State::Space => {
        self.output.write(Color::NORMAL, " ");
        self.state = State::Text;
      }
      State::Line | State::Blank => {
        for _ in 0..self.indent {
          self.output.write(Color::NORMAL, "  ");
        }
        self.state = State::Text;
      }
    }
  }

  pub fn write(&mut self, str: &str) {
    self.write_color(Color::NORMAL, str);
  }

  pub fn write_color(&mut self, color: Color, str: &str) {
    if !str.is_empty() {
      self.pre_write();
      self.output.write(color, str);
    }
  }

  pub fn write_char(&mut self, char: char) {
    self.write_color_char(Color::NORMAL, char);
  }

  pub fn write_color_char(&mut self, color: Color, char: char) {
    self.pre_write();
    self.output.write_char(color, char);
  }
}

pub trait Output {
  fn line(&mut self);
  fn write_char(&mut self, color: Color, char: char);
  fn write(&mut self, color: Color, str: &str);
}

impl Output for String {
  fn line(&mut self) {
    *self += "\n";
  }

  fn write_char(&mut self, _: Color, char: char) {
    self.push(char);
  }

  fn write(&mut self, _: Color, str: &str) {
    self.push_str(str);
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Color(pub u8, pub u8, pub u8);

impl Color {
  pub const NORMAL: Color = Color(0xbd, 0xc3, 0xc7);
  pub const STRING: Color = Color(0x6e, 0xbf, 0x9c);
  pub const COMMENT: Color = Color(0x6d, 0x72, 0x78);
  pub const KEYWORD: Color = Color(0x67, 0xc6, 0x87);
  pub const SPECIAL: Color = Color(0x92, 0xcc, 0xa5);
  pub const VAGUE: Color = Color(0x93, 0x98, 0x9d);
  pub const ERROR: Color = Color(0xda, 0x79, 0x75);
}
