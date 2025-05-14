module.exports = grammar({
  name: 'turtle',

  rules: {
    source_file: $ => seq(
      optional($.field_size),
      repeat($.command)
    ),

    field_size: $ => seq(
      $.number, 'x', $.number, /\s*\n/
    ),

    command: $ => choice(
      seq('forward', $.expr),
      seq('left', $.expr),
      seq('right', $.expr),
      seq('penup'),
      seq('pendown'),
      seq('let', $.identifier, '=', $.expr),
      seq('repeat', $.expr, '[', repeat($.command), ']')
    ),

    expr: $ => choice($.number, $.identifier),

    identifier: _ => /[a-zA-Z][a-zA-Z0-9]*/,
    number:     _ => /\d+/
  }
});
