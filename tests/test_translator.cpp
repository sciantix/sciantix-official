#include "catch.hpp"
#include "../include/LatexTranslator.h" 
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>
#include <filesystem>

extern int recursion_depth;

TEST_CASE("findMatchingBrace {}", "[utility][delimiters]") {
    SECTION("Simple match") {
        std::string s = "abc{def}ghi";
        REQUIRE(findMatchingBrace(s, 3) == 7);
    }
    SECTION("Nested braces") {
        std::string s = "a{b{c}d}e";
        REQUIRE(findMatchingBrace(s, 1) == 7);
    }
    SECTION("No matching brace") {
        std::string s = "abc{def";
        REQUIRE(findMatchingBrace(s, 3) == std::string::npos);
    }
    SECTION("Empty string") {
        std::string s = "";
        REQUIRE(findMatchingBrace(s, 0) == std::string::npos);
    }
    SECTION("Open brace not at given position") {
        std::string s = "abc(def)ghi";
        REQUIRE(findMatchingBrace(s, 3) == std::string::npos);
    }
    SECTION("Braces at start/end") {
        std::string s = "{abc}";
        REQUIRE(findMatchingBrace(s, 0) == 4);
    }
}

TEST_CASE("findMatchingBracket []", "[utility][delimiters]") {
    SECTION("Simple match") {
        std::string s = "abc[def]ghi";
        REQUIRE(findMatchingBracket(s, 3) == 7);
    }
    SECTION("Nested brackets") {
        std::string s = "a[b[c]d]e";
        REQUIRE(findMatchingBracket(s, 1) == 7);
    }
    SECTION("No matching bracket") {
        std::string s = "abc[def";
        REQUIRE(findMatchingBracket(s, 3) == std::string::npos);
    }
    SECTION("Empty string") {
        std::string s = "";
        REQUIRE(findMatchingBracket(s, 0) == std::string::npos);
    }
    SECTION("Open bracket not at given position") {
        std::string s = "abc(def)ghi";
        REQUIRE(findMatchingBracket(s, 3) == std::string::npos);
    }
}

TEST_CASE("findMatchingParen ()", "[utility][delimiters]") {
    SECTION("Simple match") {
        std::string s = "abc(def)ghi";
        REQUIRE(findMatchingParen(s, 3) == 7);
    }
    SECTION("Nested parentheses") {
        std::string s = "a(b(c)d)e";
        REQUIRE(findMatchingParen(s, 1) == 7);
    }
    SECTION("No matching parenthesis") {
        std::string s = "abc(def";
        REQUIRE(findMatchingParen(s, 3) == std::string::npos);
    }
    SECTION("Empty string") {
        std::string s = "";
        REQUIRE(findMatchingParen(s, 0) == std::string::npos);
    }
    SECTION("Open paren not at given position") {
        std::string s = "abc[def]ghi";
        REQUIRE(findMatchingParen(s, 3) == std::string::npos);
    }
}

TEST_CASE("convertExpression", "[conversion]") {

    recursion_depth = 0; 

    SECTION("Greek symbols and simple operators") {
        REQUIRE(convertExpression("\\theta + \\alpha + \\beta") == "theta+alpha+beta");
        REQUIRE(convertExpression("E=m \\cdot c^2 \\cdot \\delta") == "E=m*pow(c,2)*delta"); 
        REQUIRE(convertExpression("\\epsilon - \\phi") == "epsilon-phi");
        REQUIRE(convertExpression("\\psi / \\chi") == "psi/chi");
    }

    SECTION("Left/Right commands") {
        REQUIRE(convertExpression("\\left( x + y \\right)") == "(x+y)"); 
        REQUIRE(convertExpression("\\left(A \\cdot B\\right)") == "(A*B)");
    }

    SECTION("Trigonometric functions") {
        REQUIRE(convertExpression("\\sin{x}") == "sin(x)");
        REQUIRE(convertExpression("\\cos{alpha + beta}") == "cos(alpha+beta)");
        REQUIRE(convertExpression("\\tan{2 \\cdot pi}") == "tan(2*pi)");
        REQUIRE(convertExpression("\\sin(z)") == "sin(z)");
        REQUIRE(convertExpression("\\cos(A+B)") == "cos(A+B)");
        REQUIRE(convertExpression("\\tan C") == "tan C");
    }

    SECTION("Logarithms") {
        REQUIRE(convertExpression("\\log_{2}{x}") == "(log(x)/log(2))");
        REQUIRE(convertExpression("\\log_{b}{expression}") == "(log(expression)/log(b))");
        REQUIRE(convertExpression("\\log_{10}{Y}") == "(log(Y)/log(10))"); 
        REQUIRE(convertExpression("\\log{Z}") == "(log(Z)/log(10))");
        REQUIRE(convertExpression("\\log(Z)") == "(log(Z)/log(10))");

        REQUIRE(convertExpression("\\ln{R}") == "log(R)");
        REQUIRE(convertExpression("\\ln(S)") == "log(S)");
        REQUIRE(convertExpression("\\log_e{U}") == "log(U)");
        REQUIRE(convertExpression("\\log_e(V)") == "log(V)");
        REQUIRE(convertExpression("\\log_{e^{2}}{x}") == "(log(x)/log(exp(2)))");
    }

    SECTION("Exponentials") {
        REQUIRE(convertExpression("\\exp{x}") == "exp(x)");
        REQUIRE(convertExpression("\\exp(y+z)") == "exp(y+z)");
        REQUIRE(convertExpression("e^{k \\cdot t}") == "exp(k*t)");
        REQUIRE(convertExpression("e^{X}") == "exp(X)");
        REQUIRE(convertExpression("e^Y") == "exp(Y)");
    }

    SECTION("Subscripts and Superscripts") {
        REQUIRE(convertExpression("T_{min}") == "T_min");
        REQUIRE(convertExpression("X_{i}") == "X_i");
        REQUIRE(convertExpression("P_{crit}") == "P_crit");
        REQUIRE(convertExpression("T_{ref}") == "T_ref");

        REQUIRE(convertExpression("A_{i}^{j}") == "pow(A_i,j)"); 
        REQUIRE(convertExpression("C_{eff}^{2}") == "pow(C_eff,2)"); 
        REQUIRE(convertExpression("B_{x}^{y+z}") == "pow(B_x,y+z)"); 
        REQUIRE(convertExpression("K_{1}^{alpha}") == "pow(K_1,alpha)"); 
        REQUIRE(convertExpression("X_{sub}^{{super}}") == "pow(X_sub,{super})"); 
    }

    SECTION("Fractions") {
        REQUIRE(convertExpression("\\frac{num}{den}") == "(num/den)");
        REQUIRE(convertExpression("\\frac{1}{2}") == "(1.0/2.0)"); 
        REQUIRE(convertExpression("\\frac{X}{Y}") == "(X/Y)");
        REQUIRE(convertExpression("\\frac{A+B}{C-D}") == "((A+B)/(C-D))");
        REQUIRE(convertExpression("\\frac{\\theta}{\\alpha}") == "(theta/alpha)");
        REQUIRE(convertExpression("\\frac{1}{A+B \\cdot T}") == "(1.0/(A+B*T))");
        REQUIRE(convertExpression("\\frac{\\sqrt{x}}{y}") == "(sqrt(x)/y)");
        REQUIRE(convertExpression("\\frac{e^{k \\cdot t}}{m \\cdot c^2}") == "((exp(k*t))/(m*pow(c,2)))");
    }

    SECTION("Powers of functions") {
        REQUIRE(convertExpression("\\sin^2(x)") == "pow(sin(x),2)"); 
        REQUIRE(convertExpression("\\cos^3(theta)") == "pow(cos(theta),3)");
        REQUIRE(convertExpression("\\tan^y(z)") == "pow(tan(z),y)");
        REQUIRE(convertExpression("\\log^k(value)") == "pow((log(value)/log(10)),k)");
        REQUIRE(convertExpression("\\exp^{alpha}(val)") == "pow(exp(val),alpha)");
        REQUIRE(convertExpression("\\log_{10}^{p}(num)") == "pow((log(num)/log(10)),p)");
        REQUIRE(convertExpression("sin^{n}(x)") == "pow(sin(x),n)");
    }

    SECTION("General powers") {
        REQUIRE(convertExpression("x^2") == "pow(x,2)"); 
        REQUIRE(convertExpression("(A+B)^C") == "pow((A+B),C)");
        REQUIRE(convertExpression("T^{-1}") == "pow(T,-1)");
        REQUIRE(convertExpression("Y^{Z+1}") == "pow(Y,Z+1)");
        REQUIRE(convertExpression("e^{k \\cdot t}") == "exp(k*t)");
        REQUIRE(convertExpression("X^\\alpha") == "pow(X,alpha)");
        REQUIRE(convertExpression("X^{\\theta}") == "pow(X,theta)");
        REQUIRE(convertExpression("k^{\\frac{1}{2}}") == "pow(k,(1.0/2.0))");
    }

    SECTION("Combined and Complex Expressions") {
        REQUIRE(convertExpression("\\frac{\\sin^2(\\theta) + \\cos^2(\\theta)}{k_{avg}}") == "((pow(sin(theta),2)+pow(cos(theta),2))/k_avg)"); // Modifié : suppression espace
        REQUIRE(convertExpression("\\log_{10}{\\left( 1 + \\frac{T_{out}}{T_{in}} \\right)}") == "(log((1+(T_out/T_in)))/log(10))"); // Modifié pour log(arg)/log(base)
        REQUIRE(convertExpression("e^{\\frac{x}{y}} \\cdot \\sqrt[n]{Z}") == "exp((x/y))*pow(Z,1.0/n)");
        REQUIRE(convertExpression("pow(A_i,j+k) + (1.0 / (A + B \\cdot T))") == "pow(A_i,j+k)+(1.0/(A+B*T))"); 
    }

    SECTION("Deep expression"){
        std::string deep = "x";
        for (int i = 0; i < 400; ++i)
        deep = "\\frac{" + deep + "}{y}";
        REQUIRE_NOTHROW(convertExpression(deep));
    }
    SECTION("too Deep"){
        std::string tooDeep = "x";
        for (int i = 0; i < 5000; ++i)  
        tooDeep = "\\frac{" + tooDeep + "}{y}";
        REQUIRE_THROWS(convertExpression(tooDeep));

    }

    SECTION("Text"){
        REQUIRE(convertExpression("Hello World") == "Hello World");
    }

    SECTION("Edge Cases / Empty Inputs") {
        REQUIRE(convertExpression("") == "");
        REQUIRE(convertExpression("   ") == "");
        REQUIRE(convertExpression("x") == "x");
        REQUIRE(convertExpression("2") == "2");
    }
}