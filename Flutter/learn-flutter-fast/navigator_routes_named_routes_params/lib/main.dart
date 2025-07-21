import 'package:flutter/material.dart';
import 'package:navigator_routes_named_routes_params/customer.dart';
import 'package:navigator_routes_named_routes_params/customer_widget.dart';
import 'package:navigator_routes_named_routes_params/order.dart';
import 'package:navigator_routes_named_routes_params/order_widget.dart';

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
      home: DataContainerWidget(child: const HomePageWidget()),
      onGenerateRoute: handleRoute,
    );
  }
}

Route<dynamic> handleRoute(RouteSettings routeSettings) {
  List<String> nameParam = routeSettings.name!.split(":");
  assert(nameParam.length == 2);
  var name = nameParam[0];
  int id = int.tryParse(nameParam[1])!;
  Widget childWidget;
  if (name == "/customer/") {
    childWidget = CustomerWidget(id);
  } else {
    childWidget = OrderWidget(id);
  }
  return MaterialPageRoute(
      builder: (context) => DataContainerWidget(child: childWidget));
}

class DataContainerWidget extends InheritedWidget {
  DataContainerWidget({super.key, required Widget child}) : super(child: child);

  final _customers = [
    Customer(1, "Bike Corp", "Atlanta", [
      Order(11, DateTime(2018, 11, 17), "Bicycle parts", 197.02),
      Order(12, DateTime(2018, 12, 1), "Bicycle parts", 107.45),
    ]),
    Customer(2, "Trust Corp", "Atlanta", [
      Order(21, DateTime(2017, 1, 3), "Shredder parts", 97.02),
      Order(22, DateTime(2018, 3, 13), "Shredder blades", 7.45),
      Order(23, DateTime(2018, 5, 2), "Shredder blades", 7.45),
    ]),
    Customer(3, "Jilly Boutique", "Birmingham", [
      Order(31, DateTime(2018, 1, 3), "Display unit", 97.01),
      Order(32, DateTime(2018, 3, 3), "Desk unit", 12.25),
      Order(33, DateTime(2018, 3, 21), "Clothes rack", 97.15),
    ]),
  ];

  List<Customer> get customers => _customers;

  Customer getCustomer(int id) {
    return _customers.firstWhere((customer) => customer.id == id,
        orElse: Customer.empty);
  }

  Customer getCustomerForOrderId(int id) {
    return _customers.firstWhere(
        (customer) => _customerHasOrderId(customer, id),
        orElse: Customer.empty);
  }

  Order getOrder(int id) {
    var customer = getCustomerForOrderId(id);
    return customer.orders
        .firstWhere((order) => order.id == id, orElse: Order.empty);
  }

  bool _customerHasOrderId(Customer customer, int id) {
    var order = customer.orders
        .firstWhere((order) => order.id == id, orElse: Order.empty);
    return order.id != 0;
  }

  static DataContainerWidget of(BuildContext context) {
    return context.getInheritedWidgetOfExactType<DataContainerWidget>()!;
  }

  @override
  bool updateShouldNotify(covariant InheritedWidget oldWidget) {
    return false;
  }
}

class HomePageWidget extends StatelessWidget {
  const HomePageWidget({super.key});

  void _navigateToCustomer(BuildContext context, Customer customer) {
    Navigator.pushNamed(context, "/customer/:${customer.id}");
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
    final data = DataContainerWidget.of(context);
    final List<Widget> customers = List.from(data.customers
        .map((customer) => _createCustomerWidget(context, customer)));
    return Scaffold(
      appBar: AppBar(title: const Text("Customers")),
      body: Center(
        child: ListView(
          children: customers,
        ),
      ),
    );
  }
}
