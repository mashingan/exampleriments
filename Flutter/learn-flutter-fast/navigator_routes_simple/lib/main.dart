import 'package:flutter/material.dart';
import 'package:navigator_routes_simple/customer.dart';
import 'package:navigator_routes_simple/customer_widget.dart';
import 'package:navigator_routes_simple/order.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(primarySwatch: Colors.blue, useMaterial3: true),
      home: HomePageWidget(),
    );
  }
}

class HomePageWidget extends StatelessWidget {
  HomePageWidget({super.key});

  final _customers = [
    Customer("Bike Corp", "Atlanta", [
      Order(DateTime(2018, 11, 17), "Bicycle parts", 197.02),
      Order(DateTime(2018, 12, 1), "Bicycle parts", 107.45),
    ]),
    Customer("Trust Corp", "Atlanta", [
      Order(DateTime(2017, 1, 3), "Shredder parts", 97.02),
      Order(DateTime(2018, 3, 13), "Shredder blades", 7.45),
      Order(DateTime(2018, 5, 2), "Shredder blades", 7.45),
    ]),
    Customer("Jilly Boutique", "Birmingham", [
      Order(DateTime(2018, 1, 3), "Display unit", 97.01),
      Order(DateTime(2018, 3, 3), "Desk unit", 12.25),
      Order(DateTime(2018, 3, 21), "Clothes rack", 97.15),
    ]),
  ];

  void _navigateToCustomer(BuildContext context, Customer customer) {
    Navigator.push(context,
        MaterialPageRoute(builder: (context) => CustomerWidget(customer)));
  }

  ListTile _createCustomerWidget(BuildContext context, Customer customer) {
    return ListTile(
      title: Text(customer.name),
      subtitle: Text(customer.location),
      trailing: const Icon(Icons.arrow_right),
      onTap: () => _navigateToCustomer(context, customer),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Customers")),
      body: Center(
        child: ListView(
          children: [
            for (var c in _customers) _createCustomerWidget(context, c)
          ],
        ),
      ),
    );
  }
}
