import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:get/get.dart';
import 'package:datetime_picker_formfield/datetime_picker_formfield.dart';
import 'package:intl/intl.dart';

void main() {
  runApp(GetMaterialApp(
    title: "Flutter Demo",
    theme: ThemeData(useMaterial3: true, primarySwatch: Colors.blue),
    home: const MainApp(),
  ));
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    Get.put(PersonInfo.empty());
    return Scaffold(
      appBar: AppBar(
        title: const Text("Please enter your details"),
      ),
      body: Center(
        child: ListView(children: [
          AddressWidget(
              onSaved: _onSaved(context))
        ]),
      ),
    );
  }

  _onSaved(BuildContext context) {
    return (PersonInfo address) {
      showDialog<bool>(
          context: context,
          builder: (BuildContext ctx) {
            return AlertDialog(
              title: const Text("Address"),
              content: Text(address.toString()),
              actions: [
                TextButton(
                  onPressed: () => Get.back(result: true),
                  child: const Text("Close"),
                ),
              ],
            );
          });
    };
  }
}

class PersonInfo extends GetxController {
  var fname = "".obs;
  var lname = "".obs;
  var gender = "m".obs;
  var addr1 = "".obs;
  var addr2 = "".obs;
  var city = "".obs;
  var state = "".obs;
  var zip = "".obs;
  var fiveYears = false.obs;
  var dob = DateTime(0).obs;

  PersonInfo(
      String fname,
      String lname,
      String gender,
      String addr1,
      String addr2,
      String city,
      String state,
      String zip,
      bool fiveYears,
      DateTime dob) {
    this.fname.value = fname;
    this.lname.value = lname;
    this.gender.value = gender;
    this.addr1.value = addr1;
    this.addr2.value = addr2;
    this.city.value = city;
    this.state.value = state;
    this.zip.value = zip;
    this.fiveYears.value = fiveYears;
    this.dob.value = dob;
  }

  PersonInfo.empty();

  @override
  String toString() {
    return "PersonInfo{fname: $fname, "
    "lname: $lname, gender: $gender, "
    "addr1: $addr1, addr2: $addr2, "
    "city: $city, zip: $zip, state: $state, "
    "fiveYears: $fiveYears, dob: $dob}";
  }
}

// ignore: must_be_immutable
class AddressWidget extends StatelessWidget {
  final PersonInfo _address = Get.find();
  final ValueChanged<PersonInfo> onSaved;

  AddressWidget({
    super.key,
    required this.onSaved
  }) {
    _address.state.value = stateDropdownMenuItems[0].value!;
    _address.dob.value = DateTime.now();
    _fnameTextController =
        TextEditingController(text: _address.fname.value);
    _lnameTextController =
        TextEditingController(text: _address.lname.value);
    _gender = _address.gender.value;
    _addr1TextController =
        TextEditingController(text: _address.addr1.value);
    _addr2TextController =
        TextEditingController(text: _address.addr2.value);
    _cityTextController =
        TextEditingController(text: _address.city.value);
    _zipTextController =
        TextEditingController(text: _address.zip.value);
    _fiveYears = _address.fiveYears.value;
    _dobTextController =
        TextEditingController(text: _dateFormat.format(_address.dob.value));
  }

  static const stateDropdownMenuItems = [
    DropdownMenuItem(value: "AL", child: Text("Alabama")),
    DropdownMenuItem(value: "AK", child: Text("Alaska")),
    DropdownMenuItem(value: "AZ", child: Text("Arizona")),
    DropdownMenuItem(value: "AR", child: Text("Arkansas")),
    DropdownMenuItem(value: "CA", child: Text("California")),
    DropdownMenuItem(value: "CO", child: Text("Colorado")),
    DropdownMenuItem(value: "CT", child: Text("Connecticut")),
    DropdownMenuItem(value: "DE", child: Text("Delaware")),
    DropdownMenuItem(value: "DC", child: Text("District of Columbia")),
    DropdownMenuItem(value: "FL", child: Text("Florida")),
    DropdownMenuItem(value: "GA", child: Text("Georgia")),
    DropdownMenuItem(value: "HI", child: Text("Hawaii")),
    DropdownMenuItem(value: "ID", child: Text("Idaho")),
    DropdownMenuItem(value: "IL", child: Text("Illinois")),
    DropdownMenuItem(value: "IN", child: Text("Indiana")),
    DropdownMenuItem(value: "IA", child: Text("Iowa")),
    DropdownMenuItem(value: "KS", child: Text("Kansas")),
    DropdownMenuItem(value: "KY", child: Text("Kentucky")),
    DropdownMenuItem(value: "LA", child: Text("Louisiana")),
    DropdownMenuItem(value: "ME", child: Text("Maine")),
    DropdownMenuItem(value: "MD", child: Text("Maryland")),
    DropdownMenuItem(value: "MA", child: Text("Massachusetts")),
    DropdownMenuItem(value: "MI", child: Text("Michigan")),
    DropdownMenuItem(value: "MN", child: Text("Minnesota")),
    DropdownMenuItem(value: "MS", child: Text("Mississippi")),
    DropdownMenuItem(value: "MO", child: Text("Missouri")),
    DropdownMenuItem(value: "MT", child: Text("Montana")),
    DropdownMenuItem(value: "NE", child: Text("Nebraska")),
    DropdownMenuItem(value: "NV", child: Text("Nevada")),
    DropdownMenuItem(value: "NH", child: Text("New Hampshire")),
    DropdownMenuItem(value: "NJ", child: Text("New Jersey")),
    DropdownMenuItem(value: "NM", child: Text("New Mexico")),
    DropdownMenuItem(value: "NY", child: Text("New York")),
    DropdownMenuItem(value: "NC", child: Text("North Carolina")),
    DropdownMenuItem(value: "ND", child: Text("North Dakota")),
    DropdownMenuItem(value: "OH", child: Text("Ohio")),
    DropdownMenuItem(value: "OK", child: Text("Oklahoma")),
    DropdownMenuItem(value: "OR", child: Text("Oregon")),
    DropdownMenuItem(value: "PA", child: Text("Pennsylvania")),
    DropdownMenuItem(value: "RI", child: Text("Rhode Island")),
    DropdownMenuItem(value: "SC", child: Text("South Carolina")),
    DropdownMenuItem(value: "SD", child: Text("South Dakota")),
    DropdownMenuItem(value: "TN", child: Text("Tennessee")),
    DropdownMenuItem(value: "TX", child: Text("Texas")),
    DropdownMenuItem(value: "UT", child: Text("Utah")),
    DropdownMenuItem(value: "VT", child: Text("Vermont")),
    DropdownMenuItem(value: "VA", child: Text("Virginia")),
    DropdownMenuItem(value: "WA", child: Text("Washington")),
    DropdownMenuItem(value: "WV", child: Text("West Virginia")),
    DropdownMenuItem(value: "WI", child: Text("Wisconsin")),
    DropdownMenuItem(value: "WY", child: Text("Wyoming")),
  ];

  final _formKey = GlobalKey<FormState>();
  String _state = stateDropdownMenuItems[0].value!;
  late TextEditingController _fnameTextController;
  late TextEditingController _lnameTextController;
  String _gender = "m";
  late TextEditingController _addr1TextController;
  late TextEditingController _addr2TextController;
  late TextEditingController _cityTextController;
  late TextEditingController _zipTextController;
  bool _fiveYears = false;
  final _dateFormat = DateFormat("MMM d yyyy");
  late TextEditingController _dobTextController;

  @override
  Widget build(BuildContext context) {
    final List<Widget> forms = [
      createFNameWidget(),
      createLNameWidget(),
      createGenderWidget(),
      createAddr1Widget(),
      createAddr2Widget(),
      createCityWidget(),
      createStateWidget(),
      createZipWidget(),
      create5YearsWidget(),
      createDobWidget(),
      ElevatedButton(
          onPressed: () {
            if (_formKey.currentState!.validate()) {
              onSaved(createDataObjectFormData());
            }
          },
          child: const Text("Save"))
    ];
    return Form(key: _formKey, child: Column(children: forms,));
  }

  createFNameWidget() {
    return createNameWidget("first");
  }

  createLNameWidget() {
    return createNameWidget("last");
  }

  createNameWidget(String fl) {
    TextEditingController controller;
    if (fl == "first") {
      controller = _fnameTextController;
    } else {
      controller = _lnameTextController;
    }
    return TextFormField(
      validator: (value) {
        if (value!.isEmpty){
          return "Please enter your $fl name.";
        }
        return null;
      },
      decoration: InputDecoration(
        icon: const Icon(Icons.person),
        hintText: "First name",
        labelText: "Enter your $fl name",
      ),
      onSaved: (String? value){},
      controller: controller,
      autofocus: true,
    );
  }

  void _changeGenderRadio(String s) {
    _address.gender.value = s;
    _gender = s;
  }
  createGenderWidget() {
    final radioWidgets = [
      const Text("Male"),
      Obx(() => Radio(
          value: "m",
          groupValue: "${_address.gender}",
          onChanged: (String? s) => _changeGenderRadio(s!))),
      const Text("Female"),
      Obx(() => Radio(
          value: "f",
          groupValue: "${_address.gender}",
          onChanged: (String? s) => _changeGenderRadio(s!)))
    ];
    return InputDecorator(
      decoration: const InputDecoration(
        icon: Icon(Icons.person),
        hintText: "What is your gender?",
        labelText: "Gender",),
        child: DropdownButtonHideUnderline(child: Row(children: radioWidgets,)
      ),
    );
  }

  TextFormField createAddrWidget(String line, String hint) {
    var ctrl = line == "first" ? _addr1TextController : _addr2TextController;
    return TextFormField(
      validator: (value) {
        if (value!.isEmpty) {
          return "Please enter the $line line of your address.";
        }
        return null;
      },
      decoration: InputDecoration(
        icon: const Icon(Icons.location_city),
        hintText: hint,
        labelText: "Enter the $line line of address",
      ),
      onSaved: (String? value) {},
      controller: ctrl
    );
  }

  createAddr1Widget() {
    return createAddrWidget("first", "Address 1");
  }

  createAddr2Widget() {
    return createAddrWidget("second", "Address 2");
  }

  TextFormField createCityWidget() {
    return TextFormField(
      validator: (value) {
        if (value!.isEmpty) {
          return "Please enter your city.";
        }
        return null;
      },
      decoration: const InputDecoration(
        icon: Icon(Icons.location_city),
        hintText: "City",
        labelText: "Enter the city name",
      ),
      onSaved: (String? value) {},
      controller: _cityTextController
    );
  }

  createStateWidget() {
    return InputDecorator(
      decoration: const InputDecoration(
        icon: Icon(Icons.location_city),
        hintText: "Select the state",
        labelText: "Select the state",
      ),
      child: Obx(() => DropdownButtonHideUnderline(
          child: DropdownButton<String>(
              items: stateDropdownMenuItems,
              value: "${_address.state}",
              isDense: true,
              onChanged: (String? value) {
                _address.state.value = value!;
                _state = value;
              }))),
    );
  }

  TextFormField createZipWidget() {
    return TextFormField(
      validator: (value) {
        if (value!.isEmpty || value.length < 5) {
          return "Please enter your 5 digit zip.";
        }
        return null;
      },
      maxLength: 5,
      maxLengthEnforcement: MaxLengthEnforcement.enforced,
      keyboardType: TextInputType.phone,
      inputFormatters: [FilteringTextInputFormatter.digitsOnly],
      decoration: const InputDecoration(
        icon: Icon(Icons.location_city),
        hintText: "Zip",
        labelText: "Enter your zip",
      ),
      onSaved: (String? value) {},
      controller: _zipTextController
    );
  }

  create5YearsWidget() {
    return InputDecorator(
      decoration: const InputDecoration(
        icon: Icon(Icons.calendar_today),
        hintText: "Been at address 5 years?",
        labelText: "5 years?",
      ),
      child: DropdownButtonHideUnderline(
        child: Row(
          children: [
            Obx(() => Checkbox(
                value: _address.fiveYears.value,
                onChanged: (value) {
                  _address.fiveYears.value = value!;
                  _fiveYears = value;
                })),
            const Text("Been at address 5 years?")
          ],
        ),
      ),
    );
  }

  createDobWidget() {
    return DateTimeField(
      validator: (value){
        if (value == null) {
          return "Please enter your date of birth.";
        }
        return null;
      },
      format: _dateFormat,
      onShowPicker: (context, currentValue){
        return showDatePicker(
          context: context,
          firstDate: DateTime(2000),
          initialDate: currentValue ?? DateTime.now(),
          // lastDate: DateTime(DateTime.now().year));
          lastDate: DateTime(2030));
      },
      decoration: const InputDecoration(
        icon: Icon(Icons.date_range),
        hintText: "Date",
        labelText: "Select the date"
      ),
      controller: _dobTextController,
    );
  }

  createDataObjectFormData() {
    return PersonInfo(
        _fnameTextController.text,
        _lnameTextController.text,
        _gender,
        _addr1TextController.text,
        _addr2TextController.text,
        _cityTextController.text,
        _state,
        _zipTextController.text,
        _fiveYears,
        _dateFormat.parse(_dobTextController.text));
  }
}