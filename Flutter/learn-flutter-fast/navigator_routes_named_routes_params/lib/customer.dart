import 'package:navigator_routes_named_routes_params/order.dart';

class Customer {
  final int _id;
  final String _name;
  final String _location;
  final List<Order> _orders;

  Customer(this._id, this._name, this._location, this._orders);
  Customer.empty() : this(0, "", "", []);

  int get id => _id;
  List<Order> get orders => _orders;
  String get location => _location;
  String get name => _name;
}
