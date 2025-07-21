// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'main.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Person _$PersonFromJson(Map<String, dynamic> json) => Person(
      json['name'] as String,
      json['addr1'] as String,
      json['city'] as String,
      json['state'] as String,
    );

Map<String, dynamic> _$PersonToJson(Person instance) => <String, dynamic>{
      'name': instance.name,
      'addr1': instance.addressLine1,
      'city': instance.addressCity,
      'state': instance.addressState,
    };
