import 'package:flutter/material.dart';
import 'package:intl/intl.dart';
import 'package:navigator_routes_pageview/customer.dart';
import 'package:navigator_routes_pageview/order.dart';

class OrderWidget extends StatelessWidget {
  final Customer _customer;
  final Order _order;

  const OrderWidget(this._customer, this._order, {super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Order Info")),
      body: Padding(
        padding: const EdgeInsets.all(20),
        child: ListView(
          children: [
            Text(
              _customer.name,
              style: const TextStyle(fontSize: 30, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
            Text(
              _customer.location,
              style: const TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
            const Text(""),
            Text(
              _order.description,
              style: const TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
            Text(
              "${DateFormat('MM/dd/yyyy').format(_order.dt)}: \$${_order.total}",
              style: const TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
          ],
        ),
      ),
    );
  }
}
