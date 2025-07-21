import 'dart:convert';
import 'package:flutter/material.dart';
import 'package:get/get.dart';

void main() {
  runApp(const MainApp());
}

class MainAppState extends GetxController {
  var person = const Person("", "", "", "", []).obs;
  var error = "".obs;
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: "Flutter Demo",
      theme: ThemeData(
        primarySwatch: Colors.blue,
        useMaterial3: true,
      ),
      home: HomeWidget(),
    );
  }
}

class HomeWidget extends StatelessWidget {
  final _jsTextCtrl = TextEditingController();
  static const Person _granchild =
      Person("Tracy Brown", "9265 Roberts Avenue", "Birmingham", "AL", []);
  static const Person _adultFather = Person(
      "John Brown", "9265 Roberts Avenue", "Birmingham", "AL", [_granchild]);
  static const Person _adultNoChildren =
      Person("Jill Jones", "100 East Road", "Ocala", "FL", []);

  static const _grandfather = Person("John Brown", "9261 Roberts Avenue",
      "Birmingham", "AL", [_adultFather, _adultNoChildren]);

  HomeWidget({super.key}) {
    _jsTextCtrl.text = json.encode(_grandfather);
  }

  TextFormField _createJsonTextFormField() {
    return TextFormField(
      validator: (value) {
        if (value!.isEmpty) {
          return "Please enter the json.";
        }
        return null;
      },
      decoration: const InputDecoration(
          border: OutlineInputBorder(),
          hintText: "Json",
          labelText: "Enter the json for a person."),
      controller: _jsTextCtrl,
      autofocus: true,
      maxLines: 8,
      keyboardType: TextInputType.multiline,
    );
  }

  Function() _convertToJson(MainAppState mas) {
    return () {
      try {
        final text = _jsTextCtrl.text;
        debugPrint("JSON Text: $text");
        var decoded = json.decode(text);
        debugPrint("Decoded type: ${decoded.runtimeType}"
            ", value: $decoded");
        // mas.person.value = _$PersonFromJson(decoded);
        mas.person.value = Person.fromJson(decoded);
        debugPrint("Person type: ${mas.person.value.runtimeType}"
            ", value: ${mas.person.value}");
        mas.error.value = "";
      } catch (e) {
        debugPrint("ERROR: $e");
        mas.person.value = Person.empty();
        mas.error.value = e.toString();
      }
    };
  }

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
    _convertToJson(mas)();
    return Scaffold(
      appBar: AppBar(title: const Text("Deserialization")),
      body: Center(
        child: Padding(
          padding: const EdgeInsets.all(10),
          child: ListView(
            children: [
              _createJsonTextFormField(),
              Padding(
                padding: const EdgeInsets.only(top: 0),
                child: Obx(() {
                  return mas.error.value == ""
                      ? const Text("")
                      : Text(
                          "An error occurred:\n\n${mas.error}",
                          style: const TextStyle(color: Colors.red),
                        );
                }),
              ),
              Padding(
                padding: const EdgeInsets.only(top: 10),
                child: Obx(
                  () {
                    return mas.person.value.name == ""
                        ? const Text("Person is null")
                        : Text(
                            "Converted to Person object:\n\n${mas.person.value}");
                  },
                ),
              ),
            ],
          ),
        ),
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: _convertToJson(mas),
        tooltip: "Increment",
        child: const Icon(Icons.refresh),
      ),
    );
  }
}

class Person {
  final String name;
  final String addressLine1;
  final String addressCity;
  final String addressState;
  final List<Person> children;

  const Person(this.name, this.addressLine1, this.addressCity, this.addressState, this.children);

  Map<String, dynamic> toJson() {
    var map = {
      'name': name,
      'addr': addressLine1,
      'city': addressCity,
      'state': addressState,
      'children': children,
    };
    return map;
  }

  factory Person.fromJson(Map<String, dynamic>? json) {
    if (json == null) {
      throw const FormatException("Null JSON");
    }

    var decodedChildren = json['children'] as List<dynamic>;
    var children = <Person>[];
    for (var i = 0; i < decodedChildren.length; i++) {
      children.add(Person.fromJson(decodedChildren[i]));
    }
    return Person(json['name'], json['addr'], json['city'], json['state'], children);
  }

  static const _empty = Person("", "", "", "", []);
  factory Person.empty() {
    return _empty;
  }

  @override
  String toString() {
    return 'Person{name: $name, addressLine1: $addressLine1,'
        ' addressCity: $addressCity}, addressState: $addressState'
        ', children: [${children.join()}]}';
  }
}
