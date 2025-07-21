import 'dart:async';
import 'dart:math';
import 'package:builder_widget_streambuilder/square.dart';
import 'package:flutter/material.dart';
import 'package:rxdart/subjects.dart';

class Bloc {
  // Business logic component
  final _random = Random();
  var _squareList = <Square>[];
  final _addActionStreamController = StreamController();
  final _squareListSubject = BehaviorSubject<List<Square>>();

  Bloc() {
    _addActionStreamController.stream.listen(_handleAdd);
  }

  int next(int min, int max) => min + _random.nextInt(max - min);

  List<Square> initSquareList() {
    _squareList = [Square("Square 1", Colors.red)];
    return _squareList;
  }

  void dispose() {
    _addActionStreamController.close();
  }

  Square createSquare() {
    return Square("Square ${_squareList.length + 1}",
        Color.fromRGBO(next(0, 255), next(0, 255), next(0, 255), 0.5));
  }

  void _handleAdd(void v) {
    _squareList.add(createSquare());
    _squareListSubject.add(_squareList);
  }

  Stream<List<Square>> get squareListStream => _squareListSubject.stream;
  Sink get addAction => _addActionStreamController.sink;
}

class BlocProvider extends InheritedWidget {
  final Bloc bloc;
  const BlocProvider({Key? key, required this.bloc, required Widget child})
      : super(key: key, child: child);

  @override
  bool updateShouldNotify(InheritedWidget oldWidget) => true;

  static Bloc of(BuildContext context) =>
      (context.getInheritedWidgetOfExactType<BlocProvider>())
          !.bloc;
}
