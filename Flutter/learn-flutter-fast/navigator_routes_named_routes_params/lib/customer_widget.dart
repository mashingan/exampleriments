import 'package:flutter/material.dart';
import 'package:intl/intl.dart';
import 'package:navigator_routes_named_routes_params/main.dart';
import 'package:navigator_routes_named_routes_params/order.dart';

class CustomerWidget extends StatelessWidget {
  final int _id;

  const CustomerWidget(this._id, {super.key});

  void _navigateToOrder(BuildContext context, Order order) {
    Navigator.pushNamed(context, "/order/:${order.id}");
  }

  ListTile _createORderListWidget(BuildContext context, Order order) {
    return ListTile(
      title: Text(order.description),
      subtitle: Text(
          "${DateFormat('MM/dd/yyyy').format(order.dt)}: \$${order.total}"),
      trailing: const Icon(Icons.arrow_right),
      onTap: () => _navigateToOrder(context, order),
    );
  }

  @override
  Widget build(BuildContext context) {
    final data = DataContainerWidget.of(context);
    final customer = data.getCustomer(_id);
    final List<Widget> ordersW = List.from(customer.orders
        .map((order) => _createORderListWidget(context, order)));
    ordersW.insert(
        0,
        Container(
          padding: const EdgeInsets.all(20),
          child: Column(
            children: [
              Text(
                customer.name,
                style:
                    const TextStyle(fontSize: 30, fontWeight: FontWeight.bold),
              ),
              Text(
                customer.location,
                style:
                    const TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
              ),
              Text(
                "${customer.orders.length} order${customer.orders.length == 1 ? '' : 's'}",
                style:
                    const TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
              )
            ],
          ),
        ));
    return Scaffold(
      appBar: AppBar(title: const Text("Customer Info")),
      body: Center(
        child: ListView(
          children: ordersW,
        ),
      ),
    );
  }
}
