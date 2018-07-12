//Guita Vahdatinia
//gvahdati
#include <cstdlib>
#include <exception>
#include <stack>
#include <stdexcept>
using namespace std;

#include "ubigint.h"
#include "debug.h"

ubigint::ubigint (unsigned long that) {
   while (that > 0) {
      ubig_value.push_back(that % 10);
      that = that/10;
   }
}

ubigint::ubigint (const string& that) {
   for (int i = that.size() -1; i>=0; i--){
      char digit = that[i] - 48;
      ubig_value.push_back(digit);
   }
}

ubigint ubigint::operator+ (const ubigint& that) const {
    int lhsize = ubig_value.size(); //left hand size of vector
    int rhsize = that.ubig_value.size(); // right hand size
    int min = (lhsize < rhsize ? rhsize : lhsize); // get the min size
   int i = 0;        //  index 
   int carry = 0;     
   int digit = 0; 
   ubigint bigint(0);
    while ( i < min || carry > 0){
        if (i>=min){
            digit += carry;
        }
        else if (lhsize > i && rhsize > i ){    
            digit += ubig_value.at(i) + that.ubig_value.at(i) + carry;
        }
        else if (lhsize > i){
            digit += ubig_value.at(i) + carry;
        }
        else if (rhsize > i){
            digit += that.ubig_value.at(i) + carry;
        }
        carry = 0; 
      if (digit > 9) {
         carry = 1;
         digit = digit % 10;
      }
      bigint.ubig_value.push_back(digit);
      digit = 0;
      i++;
   }
   while (bigint.ubig_value.size() > 0 
    and bigint.ubig_value.back() == 0) 
        bigint.ubig_value.pop_back();
   return bigint;
}
ubigint ubigint::operator- (const ubigint& that) const {
   ubigint bigint(0);
    int lhsize = ubig_value.size(); //left hand size of vector
    int rhsize = that.ubig_value.size(); // right hand size
   int i = 0;        //  index 
   int digit = 0;  
    bool flag = false;
    int lhsv;
    while ( i < lhsize){ 
        if (flag){
         lhsv = ubig_value.at(i) - 1;
            flag = false;
        }   
        else lhsv = ubig_value.at(i);
        if (i<lhsize && i<rhsize){
            int rhsv = that.ubig_value.at(i);
            if(lhsv < rhsv){
                digit += (lhsv+10) - rhsv;
                flag = true;
            } else
                digit += lhsv - rhsv;
        }   
        else{
                digit += lhsv; 
    
        }   
      bigint.ubig_value.push_back(digit);
      digit = 0;
      i++;
   }  
   while (bigint.ubig_value.size() > 0 
     and bigint.ubig_value.back() == 0) 
        bigint.ubig_value.pop_back();
   return bigint;
}

ubigint ubigint::operator* (const ubigint& that) const {
   ubigint bigint(0);
    int lhsize = ubig_value.size(); //left hand size of vector
    int rhsize = that.ubig_value.size(); // right hand size
    int carry;
    int digit; 
    for (int lh = 0; lh <lhsize; lh++){
        ubigint partial(0);
        carry = 0;
        digit = 0;
        for (int rh = 0; rh < rhsize; rh ++){
            digit = (ubig_value.at(lh) * that.ubig_value.at(rh)); 
            digit+= carry;
            carry = digit / 10; 
            digit = digit % 10;
            partial.ubig_value.push_back(digit);
        }
        if(carry > 0)
            partial.ubig_value.push_back(carry);
        int zero = 0;
        while (zero<lh){
            partial.ubig_value.insert(partial.ubig_value.begin(), 0);
            zero++;
        }
        bigint = bigint + partial;
        partial.ubig_value.clear();
        }
        
   return bigint;
}
void ubigint::multiply_by_2() {
    ubigint bigint{2};
    *this = *this * bigint; 
    while (bigint.ubig_value.size() > 0 
        and bigint.ubig_value.back() == 0) 
        bigint.ubig_value.pop_back();
}
void ubigint::divide_by_2() {
   for(unsigned int i = 0; i<ubig_value.size(); i++){
       if (( i < ubig_value.size()-1) and (ubig_value.at(i+1)%2==1)){
                ubig_value.at(i) = ubig_value.at(i)/2 + 5;
        }
       else{
            ubig_value.at(i) = ubig_value.at(i)/2; 
       }
    } 
    while (ubig_value.size() > 0 and ubig_value.back() == 0) 
        ubig_value.pop_back();
}

struct quo_rem { ubigint quotient; ubigint remainder; };
quo_rem udivide (const ubigint& dividend, ubigint divisor) {
   // Note: divisor is modified so pass by value (copy).
   ubigint zero {0};
   if (divisor == zero) throw domain_error ("udivide by zero");
   ubigint power_of_2 {1};
   ubigint tester {3};
   ubigint quotient {0};
   ubigint remainder {dividend}; // left operand, dividend
   while (divisor < remainder) {
      divisor.multiply_by_2();
      power_of_2.multiply_by_2();
   }
   while (power_of_2 > zero) {
      if (divisor <= remainder) {
         remainder = remainder - divisor;
         quotient = quotient + power_of_2;
      }
      divisor.divide_by_2();
      power_of_2.divide_by_2();
   }
   return {.quotient = quotient, .remainder = remainder};
}

ubigint ubigint::operator/ (const ubigint& that) const {
   return udivide (*this, that).quotient;
}

ubigint ubigint::operator% (const ubigint& that) const {
   return udivide (*this, that).remainder;
}
bool ubigint::operator== (const ubigint& that) const {
    int lhsize = ubig_value.size(); //left hand size of vector
    int rhsize = that.ubig_value.size(); // right hand size
    if (lhsize != rhsize) return false;
    else{
        for(int i = 0; i<lhsize; i++){
            if (ubig_value.at(i)!= that.ubig_value.at(i))
                return false;
        }
    }
    return true; 
}

bool ubigint::operator< (const ubigint& that) const {
    int lhsize = ubig_value.size(); //left hand size of vector
    int rhsize = that.ubig_value.size(); // right hand size
   if (lhsize < rhsize) 
   {
        return true;
   }
   else if (lhsize > rhsize) 
   {
    return false;
   }
   else {
       for(int i = lhsize - 1; i >= 0; --i){
          if((ubig_value[i] > that.ubig_value[i])){
            return false;
            }
          if((ubig_value[i] < that.ubig_value[i])){
            return true;
          }
        }
    }
return false;
}

bool ubigint::operator<= (const ubigint& that) const {
    return ((*this<that) or (*this==that));
} 

bool ubigint::operator>= (const ubigint& that) const {
    return ((*this>that) or (*this==that));
}
bool ubigint::operator> (const ubigint& that) const {
   return not (*this<=that);
}
ostream& operator<< (ostream& out, const ubigint& that) {
   unsigned int digit = 0;
      for (int i = that.ubig_value.size()-1; i >= 0; --i) {
         if (digit%(69) == 0 and digit!= 0) out << "\\" << endl;
         out << static_cast<char>(that.ubig_value.at(i) + '0');
         digit++;
      }
   return out;
}

