class Order {
  final int _id;
  final DateTime _dt;
  final String _desc;
  final double _total;

  Order(this._id, this._dt, this._desc, this._total);
  Order.empty(): this(0, DateTime.now(), "", 0);

  int get id => _id;
  double get total => _total;
  String get description => _desc;
  DateTime get dt => _dt;
}
