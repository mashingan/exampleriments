import 'package:flutter/material.dart';
import 'package:intl/intl.dart';
import 'package:navigator_routes_named_routes_params/main.dart';

class OrderWidget extends StatelessWidget {
  final int _id;
  const OrderWidget(this._id, {super.key});

  @override
  Widget build(BuildContext context) {
    final data = context.getInheritedWidgetOfExactType<DataContainerWidget>()!;
    final customer = data.getCustomerForOrderId(_id);
    final order = data.getOrder(_id);
    return Scaffold(
      appBar: AppBar(title: const Text("Order Info")),
      body: Padding(
        padding: const EdgeInsets.all(20),
        child: ListView(
          children: [
            Text(
              customer.name,
              style: const TextStyle(fontSize: 30, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
            Text(
              customer.location,
              style: const TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
            const Text(""),
            Text(
              order.description,
              style: const TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
            Text(
              "${DateFormat('MM/dd/yyyy').format(order.dt)}: \$${order.total}",
              style: const TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
              textAlign: TextAlign.center,
            ),
          ],
        ),
      ),
    );
  }
}
