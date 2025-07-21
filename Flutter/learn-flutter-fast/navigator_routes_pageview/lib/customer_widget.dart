import 'package:flutter/material.dart';
import 'package:intl/intl.dart';
import 'package:navigator_routes_pageview/customer.dart';
import 'package:navigator_routes_pageview/order.dart';
import 'package:navigator_routes_pageview/order_widget.dart';

class CustomerWidget extends StatelessWidget {
  final Customer _customer;

  const CustomerWidget(this._customer, {super.key});

  void _navigateToOrder(BuildContext context, Order order) {
    Navigator.push(context,
        MaterialPageRoute(builder: (context) => OrderWidget(_customer, order)));
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

  List<Widget> createOrderListView(BuildContext context) {
    final List<Widget> ordersW = List.from(_customer.orders
        .map((order) => _createORderListWidget(context, order)));
    ordersW.insert(
        0,
        Container(
          padding: const EdgeInsets.all(20),
          child: Column(
            children: [
              Text(
                _customer.name,
                style:
                    const TextStyle(fontSize: 30, fontWeight: FontWeight.bold),
              ),
              Text(
                _customer.location,
                style:
                    const TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
              ),
              Text(
                "${_customer.orders.length} order${_customer.orders.length == 1 ? '' : 's'}",
                style:
                    const TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
              )
            ],
          ),
        ));
    return ordersW;
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Customer Info")),
      body: Center(
        child: ListView(children: createOrderListView(context)),
      ),
    );
  }
}
