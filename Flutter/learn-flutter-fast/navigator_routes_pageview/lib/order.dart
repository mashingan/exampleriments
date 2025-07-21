class Order {
  final DateTime _dt;
  final String _desc;
  final double _total;

  Order(this._dt, this._desc, this._total);

  double get total => _total;
  String get description => _desc;
  DateTime get dt => _dt;
}
