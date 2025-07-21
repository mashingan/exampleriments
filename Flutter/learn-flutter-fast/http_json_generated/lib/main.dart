import 'dart:convert';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:json_annotation/json_annotation.dart';

part 'main.g.dart';

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
        useMaterial3: true,
      ),
      home: HomeWidget(),
    );
  }
}

class HomeWidget extends StatelessWidget {
  static final _person =
      Person("John Brown", "9261 Roberts Avenue", "Birmingham", "AL");

  const HomeWidget({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Serialization")),
      body: Center(
        child: Padding(
          padding: const EdgeInsets.all(10),
          child: ListView(
            children: [
              Padding(
                padding: const EdgeInsets.only(top: 0),
                child: Text("Grandfather:\n$_person"),
              ),
              Padding(
                padding: const EdgeInsets.only(top: 10),
                child: Text("Json Encoded:\n${json.encode(_person)}",
                    style: const TextStyle(color: Colors.red)),
              ),
              TextButton(
                  onPressed: () {
                    Clipboard.setData(
                        ClipboardData(text: json.encode(_person)));
                  },
                  child: const Text("Copy"))
            ],
          ),
        ),
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
