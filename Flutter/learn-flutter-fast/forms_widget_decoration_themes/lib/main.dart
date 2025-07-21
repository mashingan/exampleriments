import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(
        primarySwatch: Colors.blue,
        inputDecorationTheme: const InputDecorationTheme(
            border: OutlineInputBorder(
                borderSide: BorderSide(color: Colors.blueGrey)),
            enabledBorder:
                OutlineInputBorder(borderSide: BorderSide(color: Colors.green)),
            focusedBorder: OutlineInputBorder(
                borderSide: BorderSide(color: Colors.deepPurple)),
            labelStyle: TextStyle(color: Colors.blueGrey)),
      ),
      home: HomeWidget(),
    );
  }
}

class HomeWidget extends StatelessWidget {
  final _formKey = GlobalKey<FormState>();
  final _textEditingCtrl = <TextEditingController>[];
  final _widgets = <Widget>[];

  HomeWidget({Key? key}) : super(key: key) {
    final fieldsNames = [
      {"label": "First Name", "icon": const Icon(Icons.person)},
      {"label": "Last Name", "icon": const Icon(Icons.person)},
      {"label": "Address 1", "icon": const Icon(Icons.home)},
      {"label": "Address 2", "icon": const Icon(Icons.home)},
      {"label": "City", "icon": const Icon(Icons.location_city)},
      {"label": "State", "icon": const Icon(Icons.location_on)},
      {"label": "Zip", "icon": const Icon(Icons.location_on)},
    ];
    for (int i = 0; i < fieldsNames.length; i++) {
      final fieldName = fieldsNames[i];
      final ctrl = TextEditingController(text: "");
      _textEditingCtrl.add(ctrl);
      _widgets.add(
        Padding(
          padding: const EdgeInsets.all(10.0),
          child: _createTextFormField(fieldName["label"] as String, i > 1, ctrl,
              fieldName["icon"] as Icon),
        ),
      );
    }
    _widgets.add(ElevatedButton(
        onPressed: () {
          _formKey.currentState!.validate();
        },
        child: const Text("Save")));
  }
  TextFormField _createTextFormField(
      String fname, bool enabled, TextEditingController ctrl, Icon icon) {
    return TextFormField(
      enabled: enabled,
      validator: (String? value) {
        if (value!.isEmpty) {
          return "Please enter $fname";
        }
        return null;
      },
      decoration: InputDecoration(
          icon: icon, hintText: fname, labelText: "Enter $fname"),
      controller: ctrl,
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text("Input Decoration Themes"),
      ),
      body: Padding(
        padding: const EdgeInsets.all(20),
        child: Form(
          key: _formKey,
          child: ListView(children: _widgets),
        ),
      ),
    );
  }
}
