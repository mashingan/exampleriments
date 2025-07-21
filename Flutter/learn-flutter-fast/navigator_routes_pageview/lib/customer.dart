import 'package:navigator_routes_pageview/order.dart';

class Customer {
  final String _name;
  final String _location;
  final List<Order> _orders;

  Customer(this._name, this._location, this._orders);

  List<Order> get orders => _orders;
  String get location => _location;
  String get name => _name;
}
