package main

import (
	"bytes"
	"log"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"
	"unicode"

	"github.com/gin-gonic/gin"
	"github.com/gin-gonic/gin/binding"
	"github.com/go-playground/validator"
	"github.com/mashingan/smapping"
	"github.com/stretchr/testify/assert"
)

var source = `
{
    "data_pribadi": {
        "status_pekerja": "zzzzz",
        "nama": "test",
        "nik": "1122334455667788",
        "jenis_kelamin": "1",
        "tempat_lahir": "testplace",
        "tanggal_lahir": 365,
        "negara_kelahiran": "ID",
        "agama": "zzzzz",
        "latar_belakang_pendidikan": "zzzzz",
        "status_pernikahan": "zzzzz",
        "nama_pasangan": "testpasangan",
        "nik_pasangan": "8877665544332211",
        "npwp": "112233445566789",
        "tanggal_daftar_npwp": 1000000,
        "telepon_rumah": "112233445566",
        "telepon_seluler": "112233445566",
        "email": "aluminar@temptest.com",
        "pilihan_investasi": "zzzzz",
        "punya_rumah": false,
        "no_kartu_keluarga": "1122334455667788",
        "status_hubungan_dalam_keluarga": "zzzzz",
        "nama_ibu_kandung": "testmom",
        "nama_ahli_waris": "testwaris",
        "status_ahli_waris": "zzzzz",
        "ponsel_ahli_waris": "112233445566"
    },
    "data_alamat": {
        "alamat": "Jl. jalan-jalan  no. keliling-keliling",
        "nomor_rt": "1",
        "nomor_rw": "2",
        "kelurahan": "testkelurahan",
        "kecamatan": "testkecamatan",
        "kabupaten": "testkabupaten",
        "kode_pos": "12345",
        "provinsi": "testprovinsi",
        "alamat_ahli_waris": "Jl. Keliling-keliling no. Jalan-jalan"
    },
    "data_pekerjaan": {
        "pekerjaan": "zzzzz",
        "nomor_identitas_pekerja": "",
        "jabatan": "general",
        "tanggal_mulai_bekerja": 1100000,
        "tanggal_pensiun": 0,
        "usia_pensiun": "44",
        "penghasilan_bersih": "10000000",
        "gaji_pokok": "10000000",
        "tunjangan_keluarga": "",
        "jenis_tunjangan_keluarga": "zzzzz",
        "tunjangan_jabatan": "",
        "tunjangan_kinerja": "",
        "tunjangan_lainnya": "",
        "tmt_cpns": 1000000,
        "tmt_pns": 1000000,
        "tmt_golongan": "33"
    },
    "data_kependudukan": {
    },
    "data_finansial": {
        "nomor_rekening": "112233445577888899",
        "pemilik_rekening": "temptest",
        "bank": "zzzzz",
        "cabang_bank": "zzzz-cabang"
    }
}`

type Details struct {
	PrivateInfo     `json:"data_pribadi" proto:"privateInfo"`
	AddressInfo     `json:"data_alamat" proto:"addressInfo"`
	CitizenshipInfo `json:"data_kewarganegaraan" proto:"citizenshipInfo"`
	JobInfo         `json:"data_pekerjaan" proto:"jobInfo"`
	FinancialInfo   `json:"data_finansial" proto:"financialInfo"`
}

// PrivateInfo is the information that is primarily
// attached to the candidate
type PrivateInfo struct {
	Status               string `json:"status_pekerja" proto:"status" input:"worker_status" binding:"required"`
	Name                 string `json:"nama" proto:"name" input:"name" binding:"required,max=100"`
	NIK                  string `json:"nik" proto:"NIK" input:"nik" binding:"required,max=16,numonly"`
	Gender               string `json:"jenis_kelamin" proto:"gender" input:"gender" binding:"required"`
	BirthPlace           string `json:"tempat_lahir" proto:"birthPlace" input:"birthplace" binding:"required,max=100"`
	CountryBorn          string `json:"negara_kelahiran" proto:"countryBorn" input:"nationality" binding:"required,len=2,charonly"`
	BirthDate            int64  `json:"tanggal_lahir" proto:"birthDate" input:"birth_date" binding:"required,ltnow"`
	Religion             string `json:"agama" proto:"religion" input:"religion" binding:"required"`
	Education            string `json:"latar_belakang_pendidikan" proto:"education" input:"background_education" binding:"required"`
	MaritalStatus        string `json:"status_pernikahan" proto:"maritalStatus" input:"marital_status" binding:"required"`
	Spouse               string `json:"nama_pasangan" proto:"spouse" input:"spouse_name" binding:"max=100"`
	SpouseNIK            string `json:"nik_pasangan" proto:"spouseNIK" input:"spouse_nik" binding:"len=16,numonly"`
	NPWP                 string `json:"npwp" proto:"npwp" input:"npwp" binding:"len=15,numonly"`
	NPWPRegistrationDate int64  `json:"tanggal_daftar_npwp" proto:"npwpRegistrationDate" input:"npwp_registration_date" binding:"ltnow"`
	HomePhone            string `json:"telepon_rumah" proto:"homePhone" input:"house_phone" binding:"max=30,numonly"`
	HandPhone            string `json:"telepon_seluler" proto:"handPhone" input:"cellphone" binding:"required,max=30,numonly"`
	Email                string `json:"email" proto:"email" input:"email" binding:"required,max=100,emailValidator"`
	InvestingChoice      string `json:"pilihan_investasi" proto:"investingChoice" input:"investing_type" binding:"required"`
	// gin-gonic binding tag bugs: https://github.com/gin-gonic/gin/issues/685
	HasHome               *bool  `json:"punya_rumah" proto:"hasHome" input:"own_house" binding:"required"`
	FamilyRegistrationNum string `json:"no_kartu_keluarga" proto:"familyRegistrationNum" input:"kk_number" binding:"len=16,numonly"`
	StatusInFamily        string `json:"status_hubungan_dalam_keluarga" proto:"statusInFamily" input:"family_relationship_status"`
	MotherName            string `json:"nama_ibu_kandung" proto:"motherName" input:"mother_name" binding:"required,max=100"`
	HeirName              string `json:"nama_ahli_waris" proto:"heirName" input:"heir_name" binding:"required,max=100"`
	HeirStatus            string `json:"status_ahli_waris" proto:"heirStatus" input:"heir_status" binding:"required"`
	HeirHandPhone         string `json:"ponsel_ahli_waris" proto:"heirHandPhone" input:"heir_phone_number" binding:"required,max=30,numonly"`
}

// AddressInfo is the object that identify where
// the candidate lives.
type AddressInfo struct {
	Address     string `json:"alamat" proto:"address" input:"address" binding:"required"`
	RTNum       string `json:"nomor_rt" proto:"rtNum" input:"rt_number" binding:"required,max=3,numonly"`
	RWNum       string `json:"nomor_rw" proto:"rwNum" input:"rw_number" binding:"required,max=3,numonly"`
	Village     string `json:"kelurahan" proto:"village" input:"village" binding:"required,max=100"`
	SubDistrict string `json:"kecamatan" proto:"subDistrict" input:"sub_district" binding:"required,max=100"`
	City        string `json:"kabupaten" proto:"city" input:"city" binding:"required"`
	PostalCode  string `json:"kode_pos" proto:"postalCode" input:"postal_code" binding:"required,len=5,numonly"`
	Province    string `json:"provinsi" proto:"province" input:"province" binding:"required"`
	HeirAddress string `json:"alamat_ahli_waris" proto:"heirAddress" input:"heir_address" binding:"required,max=255"`
}

// JobInfo is the object that will provide information
// about candidate's job.
type JobInfo struct {
	Job                  string `json:"pekerjaan" proto:"job" input:"occupation" binding:"required"`
	NIP                  string `json:"nomor_identitas_pekerja" proto:"nip" input:"employee_identity_number"`
	Position             string `json:"jabatan" proto:"position" input:"position" binding:"required"`
	StartedDate          int64  `json:"tanggal_mulai_bekerja" proto:"startedDate" input:"start_date"`
	RetirementDate       int64  `json:"tanggal_pensiun" proto:"retirementDate" input:"retirement_time"`
	RetirementAge        string `json:"usia_pensiun" proto:"retirementAge" input:"retirement_age_limit"`
	NetIncome            string `json:"penghasilan_bersih" proto:"netIncome" input:"net_income" binding:"max=30,numonly"`
	BaseSalary           string `json:"gaji_pokok" proto:"baseSalary" input:"salary" binding:"max=30,numonly"`
	FamilyAllowance      string `json:"tunjangan_keluarga" proto:"familyAllowance" input:"family_allowance" binding:"max=30,numonly"`
	FamilyAllowanceType  string `json:"jenis_tunjangan_keluarga" proto:"familyAllowanceType" input:"family_allowance_type"`
	PositionAllowance    string `json:"tunjangan_jabatan" proto:"positionAllowance" input:"positional_allowance" binding:"max=30,numonly"`
	PerformanceAllowance string `json:"tunjangan_kinerja" proto:"performanceAllowance" input:"performance_allowance" binding:"max=30,numonly"`
	OtherAllowance       string `json:"tunjangan_lainnya" proto:"otherAllowance" input:"other_allowance" binding:"max=30,numonly"`
	TmtCPNS              int64  `json:"tmt_cpns" proto:"tmtCPNS" input:"tmt_cpns" binding:"ltnow"`
	TmtPNS               int64  `json:"tmt_pns" proto:"tmtPNS" input:"tmt_pns" binding:"ltnow"`
	TmtCurrentRank       string `json:"tmt_golongan" proto:"tmtCurrentRank" proto:"tmt_group" input:"tmt_group" binding:"numonly"`
}

// CitizenshipInfo is the object that provides
// the information about candidate citizenship.
type CitizenshipInfo struct {
	Passport       string `json:"nomor_paspor" proto:"passport" input:"passport_number" binding:"max=50"`
	PassportExpiry int64  `json:"kadaluarsa_paspor,omitempty" proto:"passportExpiry" input:"passport_expiration_date" binding:"gtnow"`
}

// FinancialInfo is the object that provides
// candidate financial information.
type FinancialInfo struct {
	BankAccountNumber string `json:"nomor_rekening" proto:"bankAccountNumber" input:"account_number" binding:"required,max=25"`
	BankAccountOwner  string `json:"pemilik_rekening" proto:"bankAccountOwner" input:"account_name" binding:"required,max=100"`
	BankName          string `json:"bank" proto:"bankName" input:"bank_name" binding:"required,max=100"`
	BankBranchName    string `json:"cabang_bank" proto:"bankBranchName" input:"bank_branch" binding:"required,max=100"`
}

var emailValidator validator.Func = func(fl validator.FieldLevel) bool {
	email, ok := fl.Field().Interface().(string)
	var hostdom []string
	if !ok || strings.Index(email, "@") == -1 {
		return false
	}
	if hostdom = strings.Split(email, "@"); len(hostdom) > 2 {
		return false
	}
	for _, hd := range hostdom {
		for _, c := range hd {
			if c == rune('_') || c == rune('.') || c >= rune('A') || c <= rune('z') {
				continue
			} else {
				return false
			}
		}
	}
	return true
}

func compareTime(mode string) validator.Func {
	return func(fl validator.FieldLevel) bool {
		inttime, ok := fl.Field().Interface().(int64)
		if !ok {
			return false
		}
		// this is added because the field PassportExpiry seems failed
		// the validator even when not provided in the json itself.
		if inttime == 0 {
			return true
		}
		thetime := time.Unix(inttime, 0)
		now := time.Now()
		switch mode {
		case "lt":
			if thetime.Before(now) {
				return true
			}
		case "gt":
			log.Println("in gt")
			if thetime.After(now) {
				return true
			}
			log.Println(thetime, " failed")
		}
		return false

	}
}

var ltnow = compareTime("lt")
var gtnow = compareTime("gt")

func isOnly(operation func(c rune) bool) validator.Func {
	return func(fl validator.FieldLevel) bool {
		str, ok := fl.Field().Interface().(string)
		if !ok {
			return false
		}
		for _, c := range str {
			if !operation(c) {
				return false
			}
		}
		return true
	}
}

var numOnly = isOnly(unicode.IsNumber)
var charOnly = isOnly(unicode.IsLetter)
var alphanum = isOnly(func(r rune) bool {
	return unicode.IsNumber(r) || unicode.IsLetter(r)
})

var once sync.Once

func init() {
	once.Do(func() {
		if v, ok := binding.Validator.Engine().(*validator.Validate); ok {
			v.RegisterValidation("numonly", numOnly)
			v.RegisterValidation("charonly", charOnly)
			v.RegisterValidation("alphanum", alphanum)
			v.RegisterValidation("emailValidator", emailValidator)
			v.RegisterValidation("ltnow", ltnow)
			v.RegisterValidation("gtnow", gtnow)
		}
	})
}

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	r := gin.Default()
	r.POST("/", func(c *gin.Context) {
		var details Details
		if err := c.ShouldBindJSON(&details); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{
				"message": err,
			})
			return
		}
		mapdetail := smapping.MapTagsFlatten(&details, "input")
		c.JSON(http.StatusOK, mapdetail)
	})

	t := &testing.T{}
	w := httptest.NewRecorder()
	req, _ := http.NewRequest("POST", "/", bytes.NewBufferString(source))
	r.ServeHTTP(w, req)
	assert.Equal(t, 200, w.Code)
}
