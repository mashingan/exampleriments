import 'dart:convert';
import 'package:flutter/material.dart';
// ignore: depend_on_referenced_packages
import 'package:json_annotation/json_annotation.dart';
import 'package:get/get.dart';

part 'main.g.dart';

void main() {
  runApp(const MainApp());
}

class MainAppState extends GetxController {
  var person = Person("", "", "", "").obs;
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
  static final _person =
      Person("John Brown", "9261 Roberts Avenue", "Birmingham", "AL");

  HomeWidget({super.key}) {
    _jsTextCtrl.text = json.encode(_person);
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
        mas.person.value = _$PersonFromJson(decoded);
        debugPrint("Person type: ${mas.person.value.runtimeType}"
            ", value: ${mas.person.value}");
        mas.error.value = "";
      } catch (e) {
        debugPrint("ERROR: $e");
        mas.person.value = Person("", "", "", "");
        mas.error.value = e.toString();
      }
    };
  }

  @override
  Widget build(BuildContext context) {
    final MainAppState mas = Get.put(MainAppState());
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

@JsonSerializable()
class Person {
  final String name;
  @JsonKey(name: "addr1")
  final String addressLine1;
  @JsonKey(name: "city")
  final String addressCity;
  @JsonKey(name: "state")
  final String addressState;

  Person(this.name, this.addressLine1, this.addressCity, this.addressState);

  factory Person.fromJson(Map<String, dynamic> json) => _$PersonFromJson(json);
  // Map<String, dynamic> toJson() => _$PersonToJson(this);
  Map<String, dynamic> toJson() => _$PersonToJson(this);

  @override
  String toString() {
    return 'Person{name: $name, addressLine1: $addressLine1,'
        ' addressCity: $addressCity}, addressState: $addressState}';
  }
}
