package main

import (
	"fmt"
	"time"

	"github.com/mashingan/smapping"
)

type Transaction struct {
	PaymentID              int           `json:"id" gorm:"primary_key"`
	PaymentOrderID         int           `json:"payment.orderId"`
	PaymentOrder           Order         `json:"order"`
	PaymentPaymentMethodID string        `json:"methodId" gorm:"column:method"`
	PaymentPaymentMethod   PaymentMethod `json:"paymentMethod"`
	PaymentReferenceNo     string        `json:"referenceNo"`
	PaymentSequenceID      int           `json:"sequenceId"`
	PaymentBatchID         int           `json:"batchId"`
	PaymentPartnerID       int           `json:"partnerId"`
	PaymentPartner         Partner       `json:"partner"`
	PaymentAccountID       string        `json:"accountId"`
	PaymentAccountName     string        `json:"accountName"`
	PaymentWalletID        string        `json:"walletId"`
	PaymentApprovalCode    string        `json:"approvalCode"`
	PaymentStatus          int           `json:"status"`
	PaymentRemark          string        `json:"remark"`
	PaymentVoid            Void          `json:"paymentVoid"`
	PaymentCreatedAt       time.Time     `json:"createdAt"`
	PaymentCreatedBy       string        `json:"createdBy"`
	PaymentCompletedAt     time.Time     `json:"completedAt" gorm:"autoUpdateTime"`
	PaymentCompletedBy     string        `json:"completedBy"`
	OrderID                int           `json:"orderId" gorm:"primary_key"`
	OrderStoreID           int           `json:"storeId"`
	OrderStore             Store         `json:"store"`
	OrderTitle             string        `json:"title"`
	OrderCustomerID        string        `json:"customerId"`
	OrderInvoiceNo         string        `json:"invoiceNo" gorm:"unique"`
	OrderCurrency          string        `json:"currency"`
	OrderTotalPrice        float64       `json:"totalPrice"`
	OrderStatus            int           `json:"orderStatus"`
	OrderRemark            string        `json:"orderRemark"`
	OrderOrderItems        []OrderItems  `json:"orderItems"`
	OrderCreatedAt         time.Time     `json:"orderCreatedAt"`
	OrderCreatedBy         string        `json:"orderCreatedBy"`
	OrderCompletedAt       time.Time     `json:"orderModifiedAt" gorm:"autoUpdateTime"`
	OrderCompletedBy       string        `json:"orderModifiedBy"`
	OrderResolvedAs        int           `json:"resolvedAs"`
	OrderResolvedAt        time.Time     `json:"resolvedAt"`
	OrderResolvedBy        string        `json:"resolvedBy"`
}

type Order struct {
	ID          int          `json:"id" gorm:"primary_key"`
	StoreID     int          `json:"storeId"`
	Store       Store        `json:"store"`
	Title       string       `json:"title"`
	CustomerID  string       `json:"customerId"`
	InvoiceNo   string       `json:"invoiceNo" gorm:"unique"`
	Currency    string       `json:"currency"`
	TotalPrice  float64      `json:"totalPrice"`
	Status      int          `json:"status"`
	Remark      string       `json:"remark"`
	OrderItems  []OrderItems `json:"orderItems"`
	CreatedAt   time.Time    `json:"createdAt"`
	CreatedBy   string       `json:"createdBy"`
	CompletedAt time.Time    `json:"completedAt"`
	CompletedBy string       `json:"completedBy"`
	ResolvedAs  int          `json:"resolvedAs"`
	ResolvedAt  time.Time    `json:"resolvedAt"`
	ResolvedBy  string       `json:"resolvedBy"`
}

type Store struct {
	ID         int       `json:"id" gorm:"primary_key"`
	Code       string    `json:"code" gorm:"unique"`
	Name       string    `json:"name"`
	Address    string    `json:"address"`
	OriginHost string    `json:"originHost"`
	PublicKey  string    `json:"publicKey"`
	NotifURL   string    `json:"notifUrl"`
	SuccessURL string    `json:"successUrl"`
	FailedURL  string    `json:"failedUrl"`
	Status     int       `json:"status"`
	InvoicePfx string    `json:"invoicePfx"`
	CreatedAt  time.Time `json:"createdAt"`
	CreatedBy  string    `json:"createdBy"`
	ModifiedAt time.Time `json:"modifiedAt" gorm:"autoUpdateTime"`
	ModifiedBy string    `json:"modifiedBy"`
}

type OrderItems struct {
	ID          int       `json:"id" gorm:"primary_key"`
	OrderID     int       `json:"orderId"`
	GoodsID     int       `json:"goodsId"`
	Goods       Goods     `json:"goods"`
	Currency    string    `json:"currency"`
	Price       float64   `json:"price"`
	Qty         int       `json:"qty"`
	Status      int       `json:"status"`
	Remark      string    `json:"remark"`
	ExtendData  string    `json:"extendData"`
	CompletedAt time.Time `json:"CompletedAt"`
	CompletedBy string    `json:"CompletedBy"`
}

type Goods struct {
	ID          int            `json:"id" gorm:"primary_key"`
	Code        string         `json:"code" gorm:"unique"`
	Description string         `json:"description"`
	Category    string         `json:"category"`
	Cancellable int            `json:"cancellable"`
	CheckAvail  int            `json:"checkAvail"`
	Currency    string         `json:"currency"`
	PriceStart  float64        `json:"priceStart"`
	PriceEnd    float64        `json:"priceEnd"`
	Status      int            `json:"status"`
	MerchantID  int            `json:"merchantId"`
	Merchant    Merchant       `json:"merchant"`
	Stores      []Store        `json:"stores" gorm:"many2many:store_items"`
	Payments    []GoodsPayment `json:"payments"`
	CreatedAt   time.Time      `json:"createdAt"`
	CreatedBy   string         `json:"createdBy"`
	ModifiedAt  time.Time      `json:"modifiedAt" gorm:"autoUpdateTime"`
	ModifiedBy  string         `json:"modifiedBy"`
}

type Merchant struct {
	ID         int       `json:"id" gorm:"primary_key"`
	Code       string    `json:"code" gorm:"unique"`
	Name       string    `json:"name"`
	URL        string    `json:"url"`
	OriginHost string    `json:"originHost"`
	PublicKey  string    `json:"publicKey"`
	Status     int       `json:"status"`
	CreatedAt  time.Time `json:"createdAt"`
	CreatedBy  string    `json:"createdBy"`
	ModifiedAt time.Time `json:"modifiedAt" gorm:"autoUpdateTime"`
	ModifiedBy string    `json:"modifiedBy"`
}

type GoodsPayment struct {
	ID              int           `json:"id" gorm:"primary_key"`
	GoodsID         int           `json:"goodsId"`
	PaymentMethodID string        `json:"paymentMethodId" gorm:"column:payment_method"`
	PaymentMethod   PaymentMethod `json:"paymentMethod" gorm:"->"`
	PartnerID       int           `json:"partnerId"`
	Partner         Partner       `json:"partner"`
}

type Partner struct {
	ID            int       `json:"id" gorm:"primary_key"`
	Code          string    `json:"code" gorm:"unique"`
	Name          string    `json:"name"`
	Hotline       string    `json:"hotline"`
	IssuerCode    string    `json:"issuerCode"`
	URL           string    `json:"url"`
	AsyncPayment  int       `json:"asyncPayment"`
	OriginHost    string    `json:"originHost"`
	PublicKey     string    `json:"publicKey"`
	Status        int       `json:"status"`
	OrderLifetime int       `json:"orderLifetime"`
	InvoiceTmpl   int       `json:"invoiceTmpl"`
	VoidEnabled   int       `json:"voidEnabled"`
	PriceStart    float64   `json:"priceStart"`
	PriceEnd      float64   `json:"priceEnd"`
	MultiRequest  int       `json:"multiRequest"`
	CreatedAt     time.Time `json:"createdAt"`
	CreatedBy     string    `json:"createdBy"`
	ModifiedAt    time.Time `json:"modifiedAt" gorm:"autoUpdateTime"`
	ModifiedBy    string    `json:"modifiedBy"`
}

type PaymentMethod struct {
	ID          string `json:"id" gorm:"primary_key"`
	Description string `json:"description"`
}

type Void struct {
	ID           int       `json:"id" gorm:"primary_key"`
	PaymentID    int       `json:"paymentId"`
	Reason       int       `json:"reason"`
	Notes        string    `json:"notes"`
	ApprovalCode string    `json:"approvalCode"`
	Status       int       `json:"status"`
	Remark       string    `json:"remark"`
	LastAttempt  time.Time `json:"lastAttempt"`
	CreatedAt    time.Time `json:"createdAt"`
}

type Payment struct {
	ID              int           `json:"id" gorm:"primary_key"`
	OrderID         int           `json:"payment.orderId"`
	Order           Order         `json:"order"`
	PaymentMethodID string        `json:"methodId" gorm:"column:method"`
	PaymentMethod   PaymentMethod `json:"paymentMethod"`
	ReferenceNo     string        `json:"referenceNo"`
	SequenceID      int           `json:"sequenceId"`
	BatchID         int           `json:"batchId"`
	PartnerID       int           `json:"partnerId"`
	Partner         Partner       `json:"partner"`
	AccountID       string        `json:"accountId"`
	AccountName     string        `json:"accountName"`
	WalletID        string        `json:"walletId"`
	ApprovalCode    string        `json:"approvalCode"`
	Status          int           `json:"status"`
	Remark          string        `json:"remark"`
	Void            Void          `json:"void"`
	CreatedAt       time.Time     `json:"createdAt"`
	CreatedBy       string        `json:"createdBy"`
	CompletedAt     time.Time     `json:"completedAt" gorm:"autoUpdateTime"`
	CompletedBy     string        `json:"completedBy"`
}

func main() {
	payment := Payment{
		ID:      5,
		OrderID: 10,
		//Order:           Order{},
		PaymentMethodID: "GOPAY",
		//PaymentMethod:   PaymentMethod{},
		ReferenceNo: "555",
		SequenceID:  42,
		BatchID:     42,
		PartnerID:   42,
		//Partner:         Partner{},
		AccountID:    "GOPAY",
		AccountName:  "OVO",
		WalletID:     "BNI",
		ApprovalCode: "OK",
		Status:       10,
		Remark:       "No problem",
		//Void:            Void{},
		CreatedAt:   time.Now(),
		CreatedBy:   "Admin",
		CompletedAt: time.Now().AddDate(0, 0, 1),
		CompletedBy: "Admin",
	}
	fmt.Println("payment:", payment)
	transaction := Transaction{}
	jsontags := smapping.MapTags(&payment, "json")
	fmt.Println(jsontags)
	err := smapping.FillStructByTags(&transaction, jsontags, "json")
	if err != nil {
		fmt.Println("err:", err)
	}
	fmt.Println("transaction:")
	fmt.Println(transaction)
	fmt.Println("transaction.PaymentID", transaction.PaymentID, ": ", payment.ID)
	fmt.Println("transaction.PaymentOrderID", transaction.PaymentOrderID, ": ", payment.OrderID)
	fmt.Println("transaction.PaymentOrder", transaction.PaymentOrder, ": ", payment.Order)
	fmt.Println("transaction.PaymentPaymentMethodID", transaction.PaymentPaymentMethodID, ": ", payment.PaymentMethodID)
	fmt.Println("transaction.PaymentPaymentMethod", transaction.PaymentPaymentMethod, ": ", payment.PaymentMethod)
	fmt.Println("transaction.PaymentReferenceNo", transaction.PaymentReferenceNo, ": ", payment.ReferenceNo)
	fmt.Println("transaction.PaymentSequenceID", transaction.PaymentSequenceID, ": ", payment.SequenceID)
	fmt.Println("transaction.PaymentBatchID", transaction.PaymentBatchID, ": ", payment.BatchID)
	fmt.Println("transaction.PaymentPartnerID", transaction.PaymentPartnerID, ": ", payment.PartnerID)
	fmt.Println("transaction.PaymentPartner", transaction.PaymentPartner, ": ", payment.Partner)
	fmt.Println("transaction.PaymentAccountID", transaction.PaymentAccountID, ": ", payment.AccountID)
	fmt.Println("transaction.PaymentAccountName", transaction.PaymentAccountName, ": ", payment.AccountName)
	fmt.Println("transaction.PaymentWalletID", transaction.PaymentWalletID, ": ", payment.WalletID)
	fmt.Println("transaction.PaymentApprovalCode", transaction.PaymentApprovalCode, ": ", payment.ApprovalCode)
	fmt.Println("transaction.PaymentStatus", transaction.PaymentStatus, ": ", payment.Status)
	fmt.Println("transaction.PaymentRemark", transaction.PaymentRemark, ": ", payment.Remark)
	fmt.Println("transaction.PaymentCreatedAt", transaction.PaymentCreatedAt, ": ", payment.CreatedAt)
	fmt.Println("transaction.PaymentCreatedBy", transaction.PaymentCreatedBy, ": ", payment.CreatedBy)
	fmt.Println("transaction.PaymentCompletedAt", transaction.PaymentCompletedAt, ": ", payment.CompletedAt)
	fmt.Println("transaction.PaymentCompletedBy", transaction.PaymentCompletedBy, ": ", payment.CompletedBy)
	fmt.Println("transaction.OrderID", transaction.OrderID, ": ", payment.Order.ID)
	fmt.Println("transaction.OrderStoreID", transaction.OrderStoreID, ": ", payment.Order.StoreID)
	fmt.Println("transaction.OrderStore", transaction.OrderStore, ": ", payment.Order.Store)
	fmt.Println("transaction.OrderTitle", transaction.OrderTitle, ": ", payment.Order.Title)
	fmt.Println("transaction.OrderCustomerID", transaction.OrderCustomerID, ": ", payment.Order.CustomerID)
	fmt.Println("transaction.OrderInvoiceNo", transaction.OrderInvoiceNo, ": ", payment.Order.InvoiceNo)
	fmt.Println("transaction.OrderCurrency", transaction.OrderCurrency, ": ", payment.Order.Currency)
	fmt.Println("transaction.OrderTotalPrice", transaction.OrderTotalPrice, ": ", payment.Order.TotalPrice)
	fmt.Println("transaction.OrderStatus", transaction.OrderStatus, ": ", payment.Order.Status)
	fmt.Println("transaction.OrderRemark", transaction.OrderRemark, ": ", payment.Order.Remark)
	fmt.Println("transaction.OrderOrderItems", transaction.OrderOrderItems, ": ", payment.Order.OrderItems)
	fmt.Println("transaction.OrderCreatedAt", transaction.OrderCreatedAt, ": ", payment.Order.CreatedAt)
	fmt.Println("transaction.OrderCreatedBy", transaction.OrderCreatedBy, ": ", payment.Order.CreatedBy)
	fmt.Println("transaction.OrderCompletedAt", transaction.OrderCompletedAt, ": ", payment.Order.CompletedAt)
	fmt.Println("transaction.OrderCompletedBy", transaction.OrderCompletedBy, ": ", payment.Order.CompletedBy)
	fmt.Println("transaction.OrderResolvedAs", transaction.OrderResolvedAs, ": ", transaction.OrderResolvedAs, ": ", payment.Order.ResolvedAs)
	fmt.Println("transaction.OrderResolvedAt", transaction.OrderResolvedAt, ": ", payment.Order.ResolvedAt)
	fmt.Println("transaction.OrderResolvedBy", transaction.OrderResolvedBy, ": ", payment.Order.ResolvedBy)
}
