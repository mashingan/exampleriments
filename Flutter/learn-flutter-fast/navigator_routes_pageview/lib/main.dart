import 'package:flutter/material.dart';
import 'package:navigator_routes_pageview/customer.dart';
import 'package:navigator_routes_pageview/customer_widget.dart';
import 'package:navigator_routes_pageview/order.dart';

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

  final PageController _pageController = PageController(initialPage: 0);
  final Duration _duration = const Duration(seconds: 1);
  final Curve _curve = Curves.ease;

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

  Widget pageViewItemBuilder(BuildContext context, int index) {
    if (index == 0) {
      return createHomePage(context);
    }
    return createDetailPage(context, index);
  }

  Widget createHomePage(BuildContext context) {
    var widgets = <Widget>[];
    widgets.add(const Padding(
      padding: EdgeInsets.all(20),
      child: Text(
        "Customer List",
        style: TextStyle(fontSize: 30, fontWeight: FontWeight.bold),
        textAlign: TextAlign.center,
      ),
    ));
    for (int i = 0, ii = _customers.length; i < ii; i++) {
      var c = _customers[i];
      widgets.add(createHomePageListItem(context, c, i));
    }
    return ListView(
      children: widgets,
    );
  }

  ListTile createHomePageListItem(
      BuildContext context, Customer customer, int index) {
    return ListTile(
        title: Text(customer.name),
        subtitle: Text(customer.location),
        trailing: const Icon(Icons.arrow_right),
        onTap: () => _pageController.animateToPage(index + 1,
            duration: _duration, curve: _curve));
  }

  Widget createDetailPage(BuildContext context, int index) {
    var c = _customers[index-1];
    return ListView(children: CustomerWidget(c).createOrderListView(context),);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Customers"),
      actions: [
        IconButton(icon: const Icon(Icons.home),
        onPressed: () => _pageController.animateToPage(0, duration: _duration, curve: _curve),
        )
      ],),
      body: Center(
        child: PageView.builder(
          controller: _pageController,
          itemCount: _customers.length+1,
          itemBuilder: pageViewItemBuilder),
      ),
    );
  }
}
