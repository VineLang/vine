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

  pub fn space(&mut self) {
    match self.state {
      State::Text | State::Space => self.state = State::Space,
      State::Line | State::Blank => {}
    }
  }

  pub fn line(&mut self) {
    match self.state {
      State::Text | State::Space => {
        self.output.line();
        self.state = State::Line;
      }
      State::Line | State::Blank => {}
    }
  }

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

  fn pre_write(&mut self) {
    match self.state {
      State::Text => {}
      State::Space => {
        self.output.write(Color::WHITE, " ");
        self.state = State::Text;
      }
      State::Line | State::Blank => {
        for _ in 0..self.indent {
          self.output.write(Color::WHITE, "  ");
        }
        self.state = State::Text;
      }
    }
  }

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

  pub fn write(&mut self, str: &str) {
    self.write_color(Color::WHITE, str);
  }

  pub fn write_color(&mut self, color: Color, str: &str) {
    if !str.is_empty() {
      self.pre_write();
      self.output.write(color, str);
    }
  }

  pub fn write_char(&mut self, char: char) {
    self.write_color_char(Color::WHITE, char);
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
  pub const WHITE: Color = Color(0xbd, 0xc3, 0xc7);
  pub const STRING: Color = Color(0x6e, 0xbf, 0x9c);
  pub const COMMENT: Color = Color(0x6d, 0x72, 0x78);
  pub const KEYWORD: Color = Color(0x67, 0xc6, 0x87);
  pub const SPECIAL: Color = Color(0x92, 0xcc, 0xa5);
  pub const VAGUE: Color = Color(0x93, 0x98, 0x9d);
  pub const ERROR: Color = Color(0xda, 0x79, 0x75);
}
