package main

import (
	"database/sql"
	"log"

	_ "github.com/lib/pq"
	"github.com/mashingan/smapping"
)

type fetchDetail struct {
	Status                string `json:"status_pekerja" proto:"status"`
	Name                  string `json:"nama" proto:"name"`
	NIK                   string `json:"nik" proto:"Nik"`
	Gender                string `json:"jenis_kelamin" proto:"gender"`
	BirthPlace            string `json:"tempat_lahir" proto:"birthPlace"`
	Nationality           string `json:"kewarganegaraan" proto:"countryBorn"`
	BirthDate             int64  `json:"tanggal_lahir" proto:"birthDate"`
	BirthCountry          string `json:"negara_kelahiran" proto:"birthCountry"`
	Religion              string `json:"agama" proto:"religion"`
	Education             string `json:"latar_belakang_pendidikan" proto:"education"`
	MaritalStatus         string `json:"status_pernikahan" proto:"maritalStatus"`
	Spouse                string `json:"nama_pasangan" proto:"spouse"`
	SpouseNIK             string `json:"nik_pasangan" proto:"spouseNIK"`
	NPWP                  string `json:"npwp" proto:"npwp"`
	NPWPRegistrationDate  int64  `json:"tanggal_daftar_npwp" proto:"npwpRegistrationDate"`
	HomePhone             string `json:"telepon_rumah" proto:"homePhone"`
	HandPhone             string `json:"telepon_seluler" proto:"handPhone"`
	Email                 string `json:"email" proto:"email"`
	InvestingChoice       string `json:"pilihan_investasi" proto:"investingChoice"`
	HasHome               bool   `json:"punya_rumah" proto:"hasHome"`
	FamilyRegistrationNum string `json:"no_kartu_keluarga" proto:"familyRegistrationNum"`
	StatusInFamily        string `json:"status_hubungan_dalam_keluarga" proto:"statusInFamily"`
	MotherName            string `json:"nama_ibu_kandung" proto:"motherName"`
	HeirName              string `json:"nama_ahli_waris" proto:"heirName"`
	HeirStatus            string `json:"status_ahli_waris" proto:"heirStatus"`
	HeirHandPhone         string `json:"ponsel_ahli_waris" proto:"heirHandPhone"`
	Address               string `json:"alamat" proto:"address"`
	RTNum                 string `json:"nomor_rt" proto:"rtNum"`
	RWNum                 string `json:"nomor_rw" proto:"rwNum"`
	Village               string `json:"kelurahan" proto:"village"`
	SubDistrict           string `json:"kecamatan" proto:"subDistrict"`
	City                  string `json:"kabupaten" proto:"city"`
	PostalCode            string `json:"kode_pos" proto:"postalCode"`
	Province              string `json:"provinsi" proto:"province"`
	HeirAddress           string `json:"alamat_ahli_waris" proto:"heirAddress"`
	Job                   string `json:"pekerjaan" proto:"job"`
	NIP                   string `json:"nomor_identitas_pekerja" proto:"nip"`
	InstitutionName       string `json:"nama_institusi" proto:"institutionName"`
	Position              string `json:"jabatan" proto:"position"`
	StartedDate           int64  `json:"tanggal_mulai_bekerja" proto:"startedDate"`
	RetirementDate        int64  `json:"tanggal_pensiun" proto:"retirementDate"`
	RetirementAge         string `json:"usia_pensiun" proto:"retirementAge"`
	NetIncome             string `json:"penghasilan_bersih" proto:"netIncome"`
	BaseSalary            string `json:"gaji_pokok" proto:"baseSalary"`
	FamilyAllowance       string `json:"tunjangan_keluarga" proto:"familyAllowance"`
	FamilyAllowanceType   string `json:"jenis_tunjangan_keluarga" proto:"familyAllowanceType"`
	PositionAllowance     string `json:"tunjangan_jabatan" proto:"positionAllowance"`
	PerformanceAllowance  string `json:"tunjangan_kinerja" proto:"performanceAllowance"`
	OtherAllowance        string `json:"tunjangan_lainnya" proto:"otherAllowance"`
	TmtCPNS               int64  `json:"tmt_cpns" proto:"tmtCPNS"`
	TmtPNS                int64  `json:"tmt_pns" proto:"tmtPNS"`
	TmtCurrentRank        string `json:"tmt_golongan" proto:"tmtCurrentRank"`
	AsnCurrentRank        string `json:"asn_golongan" proto:"asnCurrentRank"`
	WorkUnit              string `json:"unit_kerja" proto:"workUnit"`
	Passport              string `json:"nomor_paspor" proto:"passport"`
	PassportExpiry        int64  `json:"kadaluarsa_paspor,omitempty" proto:"passportExpiry"`
	BankAccountNumber     string `json:"nomor_rekening" proto:"bankAccountNumber"`
	BankAccountOwner      string `json:"pemilik_rekening" proto:"bankAccountOwner"`
	BankName              string `json:"bank" proto:"bankName"`
	BankBranchName        string `json:"cabang_bank" proto:"bankBranchName"`
}

func main() {
	db, err := sql.Open("postgres", "postgresql://sitaraapp:br5KGBdM2s@10.172.31.20:26257/sitaracore_dev")
	if err != nil {
		log.Fatal(err)
	}

	query := `
SELECT
cp.status_pendaftaran status_pekerja,
cp.nama_lengkap nama,
cp.nik nik,
cp.jenis_kelamin,
cp.tempat_lahir,
cp.kewarganegaraan,
cp.tanggal_lahir::int64,
COALESCE(data_peserta ->> 'birth_country', '') negara_kelahiran,
COALESCE(data_peserta ->> 'religion', '') agama,
COALESCE(data_peserta ->> 'background_education', '') latar_belakang_pendidikan,
COALESCE(data_peserta ->> 'marital_status', '') status_pernikahan,
COALESCE(data_peserta ->> 'spouse_name', '') nama_pasangan,
COALESCE(data_peserta ->> 'spouse_nik', '') nik_pasangan,
COALESCE(data_peserta ->> 'npwp', '') npwp,
COALESCE((data_peserta ->> 'npwp_registration_date')::int64, 0) tanggal_daftar_npwp,
COALESCE(data_peserta ->> 'house_phone', '') telepon_rumah,
COALESCE(data_peserta ->> 'cellphone', '') telepon_seluler,
COALESCE(data_peserta ->> 'email', '') email,
COALESCE(data_peserta ->> 'investation_type', '') pilihan_investasi,
COALESCE((data_peserta ->> 'own_house')::bool, 'no') punya_rumah,
COALESCE(data_peserta ->> 'kk_number', '') no_kartu_keluarga,
COALESCE(data_peserta ->> 'family_relationship_status', '') status_hubungan_dalam_keluarga,
COALESCE(data_peserta ->> 'mother_name', '') nama_ibu_kandung,
COALESCE(data_peserta ->> 'heir_name', '') nama_ahli_waris,
COALESCE(data_peserta ->> 'heir_status', '') status_ahli_waris,
COALESCE(data_peserta ->> 'heir_phone_number', '') ponsel_ahli_waris,
COALESCE(data_peserta ->> 'address', '') alamat,
COALESCE(data_peserta ->> 'rt_number', '') nomor_rt,
COALESCE(data_peserta ->> 'rw_number', '') nomor_rw,
COALESCE(data_peserta ->> 'village', '') kelurahan,
COALESCE(data_peserta ->> 'sub_district', '') kecamatan,
COALESCE(data_peserta ->> 'city', '') kabupaten,
COALESCE(data_peserta ->> 'postal_code', '') kode_pos,
COALESCE(data_peserta ->> 'province', '') provinsi,
COALESCE(data_peserta ->> 'heir_address', '') alamat_ahli_waris,
COALESCE(data_peserta ->> 'occupation', '') pekerjaan,
COALESCE(data_peserta ->> 'employee_identity_number', '') nomor_identitas_pekerja,
pk.nama_institusi,
COALESCE(data_peserta ->> 'position', '') jabatan,
COALESCE((data_peserta ->> 'start_time')::int64, 0) tanggal_mulai_bekerja,
COALESCE((data_peserta ->> 'retirement_time')::int64, 0) tanggal_pensiun,
COALESCE(data_peserta ->> 'retirement_age', '') usia_pensiun,
COALESCE(data_peserta ->> 'net_income', '') penghasilan_bersih,
COALESCE(data_peserta ->> 'salary', '') gaji_pokok,
COALESCE(data_peserta ->> 'family_allowance', '') tunjangan_keluarga,
COALESCE(data_peserta ->> 'family_allowance_type', '') jenis_tunjangan_keluarga,
COALESCE(data_peserta ->> 'position_allowance', '') tunjangan_jabatan,
COALESCE(data_peserta ->> 'performance_allowance', '') tunjangan_kinerja,
COALESCE(data_peserta ->> 'other_allowance', '') tunjangan_lainnya,
COALESCE((data_peserta ->> 'tmt_cnps')::int64, 0) tmt_cpns,
COALESCE((data_peserta ->> 'tmt_cnps')::int64, 0) tmt_pns,
COALESCE(data_peserta ->> 'tmt_group', '') tmt_golongan,
COALESCE(data_peserta ->> 'asn_group', '') asn_golongan,
COALESCE(data_peserta ->> 'work_unit', '') unit_kerja,
COALESCE(data_peserta ->> 'passport_number', '') nomor_paspor,
COALESCE((data_peserta ->> 'passport_expiry_date')::int64, 0) kadaluarsa_paspor,
COALESCE(data_peserta ->> 'account_number', '') nomor_rekening,
COALESCE(data_peserta ->> 'account_name', '') pemilik_rekening,
COALESCE(data_peserta ->> 'bank_name', '') bank,
COALESCE(data_peserta ->> 'bank_branch', '') cabang_bank
FROM calon_peserta cp JOIN pemberi_kerja pk
ON cp.pemberi_kerja = pk.id WHERE cp.id::string = $1`
	id := "0185a774-1df8-4998-b67d-9e6524343b2a"
	detailObj := fetchDetail{}
	if err := smapping.SQLScan(db.QueryRow(query, id),
		&detailObj, "json"); err != nil {
		log.Fatal(err)
	}
	log.Println("detailObj:", detailObj)
}
