import 'package:flutter/material.dart';

class Square {
  final String _text;
  final Color _color;

  Square(this._text, this._color);

  @override
  operator ==(other) =>
      (other is Square) && (_text == other._text) && (_color == other._color);

  @override
  int get hashCode => _text.hashCode ^ _color.hashCode;

  Color get color => _color;
  String get text => _text;
}
