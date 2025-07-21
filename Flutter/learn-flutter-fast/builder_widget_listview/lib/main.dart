import 'package:builder_widget_listview/_nasa_offices.dart';
import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});
  @override
  Widget build(BuildContext context) {
    var builder = ListView.builder(
      itemCount: nasaOffices.length,
      itemBuilder: (context, index) {
        debugPrint("invoking itemBuilder for row $index");
        var nasaOffice = nasaOffices[index];
        return ListTile(
          title: Text(nasaOffice['Name']!),
          subtitle: Text("${nasaOffice['Address']}: ${nasaOffice['City']}, "
              "${nasaOffice['State']} - ${nasaOffice['ZIP']} ${nasaOffice['Country']}"),
          trailing: const Icon(Icons.arrow_right),
        );
      },
    );
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(primarySwatch: Colors.blue, useMaterial3: true),
      home: Scaffold(
        appBar: AppBar(title: const Text("Nasa Offices"),),
        body: Center(
          child: builder,
        ),
      ),
    );
  }
}
