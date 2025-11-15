#include "hw7.h"

//helper function for temp naming 
static int temp_count = 0;

static char next_temp_name() {
    char name= (char)('a'+(temp_count%26)); //wraps around the alphabet
    temp_count++;
    return name;
}

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (!root) 
	{
        bst_sf *node= malloc(sizeof(bst_sf)); 
        node->mat= mat;
        node->left_child= node->right_child= NULL; //set children to null
        return node;
    }
    if (mat->name < root->mat->name) 
	{
        root->left_child= insert_bst_sf(mat, root->left_child);
    } 
	else 
	{
        root->right_child= insert_bst_sf(mat, root->right_child);
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
     while (root) 
	 {
        if (name== root->mat->name) 
		{
			return root->mat; //found the bst
		}
        if (name < root->mat->name) 
		{
			root= root->left_child; //go left down the bst
		}
        else 
		{
			root= root->right_child; //go right down the bst
		}
    }
    return NULL;
}

void free_bst_sf(bst_sf *root) {
	if (!root) 
	{
		return;
	}
    free_bst_sf(root->left_child); //recursively free left child, right child, then the root
    free_bst_sf(root->right_child);
    if (root->mat) 
	{
		free(root->mat);
	}
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int r= mat1->num_rows; //get row and col num
    unsigned int c= mat1->num_cols;
    matrix_sf *out= malloc(sizeof(matrix_sf)+r*c*sizeof(int)); //make new matrix
    out->num_rows= r;
    out->num_cols= c;
    out->name= next_temp_name();
    for (unsigned int i=0;i<r*c i++)
	{
        out->values[i] = mat1->values[i] + mat2->values[i]; //sum the two matrix into the new matrix
	}
    return out;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
	unsigned int r= mat1->num_rows;
    unsigned int c= mat2->num_cols;
    unsigned int kMax= mat1->num_cols; //same col for mat1 and row for mat2
    matrix_sf *out= malloc(sizeof(matrix_sf)+r*c*sizeof(int));
    out->num_rows= r;
    out->num_cols= c;
    out->name = next_temp_name();
    for (unsigned int i=0;i<r;i++) 
	{
        for (unsigned int j=0;j<c;j++) 
		{
            long long sum = 0; //safety for int multiplication/ prevents overflow
            for (unsigned int k=0;k<kMax;k++)
			{
                sum+= (long long)mat1->values[i*kMax+k]*mat2->values[k*c+j]; //matrix multiplcation 
			}
            out->values[i*c+j]= (int)sum; //storing into new matrix
        }
    }
    return out;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    unsigned int r= mat->num_rows;
    unsigned int c= mat->num_cols;
    matrix_sf *out= malloc(sizeof(matrix_sf)+r*c*sizeof(int));
    out->num_rows= c; //reversing/flipping the row and col
    out->num_cols = r;
    out->name = next_temp_name();
    for (unsigned int i=0;i<r;i++)
	{
        for (unsigned int j=0;j<c;j++)
		{
            out->values[j*r+i]= mat->values[i*c+j]; //putting each col into rows
		}
	}
    return out;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *p= expr;
    while (*p&&isspace((unsigned char)*p)) 
		p++; //skip white spaces
    long nr= strtol(p, (char**)&p, 10); //read row num
    while (*p&&isspace((unsigned char)*p)) 
		p++;//skip white spaces
    long nc= strtol(p, (char**)&p, 10); //read col num
    while (*p&&*p != '[') 
		p++; //skip to [
    if (!*p) 
		return NULL;
    p++; 
    matrix_sf *mat = malloc(sizeof(matrix_sf) + nr*nc*sizeof(int));
    mat->name = name;
    mat->num_rows = nr;
    mat->num_cols = nc;
    int count = 0;
    while (*p&&count < nr*nc) {   //parse values
        while (*p&&isspace((unsigned char)*p)) 
			p++;
        if (*p == ';') {
			p++; continue; 
		}
        long val = strtol(p, (char**)&p, 10);
        mat->values[count++] = (int)val;
    }
    if (count != nr*nc) { 
		free(mat); return NULL; 
		} //not enough num filled for matrix
    return mat;
}

char* infix2postfix_sf(char *infix) {
    size_t len= strlen(infix);
    char *post= malloc(len+1); //output string
    char stack[128];
    int top= -1; //stack pointer
    size_t out= 0; //output string index
    for (size_t i=0; i<len; i++) { //loop thorugh expression
        char c = infix[i];
        if (isspace((unsigned char)c)) 
			continue;
        if (isupper((unsigned char)c)) { 
			post[out++] = c; continue; } //operand for uppercase letters
        if (c == '\'') { 
			post[out++] = '\''; continue; } //transpose operator 
        if (c == '(') { 
			stack[++top] = '('; continue; } //left paranthesis
        if (c == ')') { 
			//right paranthesis
            while (top >=0 && stack[top]!='(') 
				post[out++] = stack[top--]; //pop until left parenthesis
            if (top >=0) 
				top--; // remove '('
            continue;
        }
        int prec= (c=='+'?1:2); //set operator precedence
        while (top >=0 && stack[top]!='(') { //pop out high precedence operators
            int top_prec = (stack[top]=='+'?1:(stack[top]=='*'?2:0));
            if (top_prec >= prec) 
				post[out++] = stack[top--];
            else 
				break;
        }
        stack[++top] = c; //push lower precedence operator
    }
    while (top>=0) 
		post[out++] = stack[top--]; //pop out rest of stack
    post[out] = '\0'; //add null terminator
    return post;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    return NULL;
}

matrix_sf *execute_script_sf(char *filename) {
   return NULL;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
