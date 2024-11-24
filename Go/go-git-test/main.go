package main

import (
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"github.com/go-git/go-git/v5"
	"github.com/go-git/go-git/v5/plumbing"
	"github.com/go-git/go-git/v5/plumbing/object"
)

const (
	smappingrepo = "https://github.com/mashingan/smapping"
	smappingpath = "smapping"
	dummytxt     = "dummy.txt"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	r, err := git.PlainClone(smappingpath, false, &git.CloneOptions{
		URL: smappingrepo,
	})
	if err != nil {
		log.Println(err)
		return
	}
	defer os.RemoveAll(smappingpath)
	headref, err := r.Head()
	if err != nil {
		log.Println(err)
		return
	}
	ref := plumbing.NewHashReference("refs/heads/new-branch", headref.Hash())
	r.Storer.SetReference(ref)
	w, _ := r.Worktree()
	os.WriteFile(smappingpath+"/"+dummytxt, []byte("Hello い世界"), 0644)
	w.Add(dummytxt)
	log.Println(w.Status())
	opt := &git.CommitOptions{
		Author: &object.Signature{
			Name:  "John Doe",
			Email: "jd@op",
			When:  time.Now(),
		},
	}
	commit, err := w.Commit("add dummy.txt into new branch", opt)
	if err != nil {
		log.Println(err)
		return
	}
	log.Println(r.CommitObject(commit))
	basecommit, err := r.CommitObject(headref.Hash())
	if err != nil {
		log.Println(err)
		return
	}
	newcommit, err := r.CommitObject(commit)
	if err != nil {
		log.Println(err)
		return
	}
	res, err := basecommit.MergeBase(newcommit)
	if err != nil {
		log.Println(err)
		return
	}
	for _, commit := range res {
		fmt.Printf(
			"\x1b[36;1m%s \x1b[90;21m%s\x1b[0m %s\n",
			commit.Hash.String()[:7],
			commit.Hash.String(),
			strings.Split(commit.Message, "\n")[0],
		)
	}
	latesthead, err := r.Head()
	if err != nil {
		log.Println(err)
		return
	}
	since := time.Now().Add(-time.Hour * 24 * 30)
	until := time.Now()
	citer, err := r.Log(&git.LogOptions{
		From:  latesthead.Hash(),
		Since: &since,
		Until: &until,
	})
	if err != nil {
		log.Println(err)
		return
	}
	citer.ForEach(func(c *object.Commit) error {
		log.Println(c)
		return nil
	})
}
