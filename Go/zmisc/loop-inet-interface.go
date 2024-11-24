package main

import (
	"fmt"
	"net"
)

func main() {

	ip, err := externalIP()
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(ip)

}

func skipLoopbackAndOffline(iface net.Interface) bool {
	return (iface.Flags&net.FlagUp == 0) && (iface.Flags&net.FlagLoopback != 0)
}

func retrieveIp(addr net.Addr) (ipstring string, error error) {
	var ip net.IP
	switch v := addr.(type) {
	case *net.IPNet:
		ip = v.IP
	case *net.IPAddr:
		ip = v.IP
	}
	if ip == nil || ip.IsLoopback() {
		return
	}
	ip = ip.To4()
	if ip == nil {
		return
	}
	ipstring = ip.String()
	return

}

func externalIP() (string, error) {
	ifaces, err := net.Interfaces()
	if err != nil {
		return "", err
	}
	for _, iface := range ifaces {
		addrs, err := iface.Addrs()
		if err != nil {
			return "", err
		}
		if skipLoopbackAndOffline(iface) {
			continue
		}
		for _, addr := range addrs {
			ipstring, err := retrieveIp(addr)
			if ipstring == "" || err != nil {
				continue
			}
			return ipstring, err
		}
	}
	return "", fmt.Errorf("are you connected to the network?")
}
