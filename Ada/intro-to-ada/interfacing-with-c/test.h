struct test;

struct test * test_create();

void test_destroy(struct test *);

void test_reset(struct test *);

void test_set_name(struct test *, char *);

void test_set_address(struct test *, char *);

void test_display(const struct test *);
