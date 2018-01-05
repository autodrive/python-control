"""
Find slycot functions directly or indirectly called in python-control
Example: $ python find_slycot.py [path to python-control] [path to slycot]
"""
import copy
import os
import re
from pprint import pprint

import numpy.linalg.lapack_lite as np_lapack_lite
import scipy.linalg.blas as blas
import scipy.linalg.cython_blas as cython_blas
import scipy.linalg.cython_lapack as cython_lapack
import scipy.linalg.lapack as lapack


def get_first_script_parameter():
    """
    If first argument of script not given, use current directory
    Returns
    -------

    """
    from sys import argv
    result = os.curdir
    if 1 < len(argv):
        result = argv[1]
    return os.path.abspath(result)


class RecursiveFinder(object):
    """
    Search for pattern in files of given extension recursively
    """

    def __init__(self, initial_path=os.curdir, extension='.py', pattern="from" + " slycot import", b_rel_path=True):
        self.abs_return_path = os.getcwd()
        self.extension = extension
        self.pattern = pattern

        self.result = {}

        abs_initial_path = os.path.abspath(initial_path)
        self.b_rel_path_key = b_rel_path

        self.ignore_if_folder_parts_include_set = {'.git', '.idea', 'build', 'slycot_conversion'}

        if not os.path.exists(abs_initial_path):
            raise IOError('File does not exist: %s' % abs_initial_path)
        elif not os.path.isdir(abs_initial_path):
            raise IOError('Not a directory: %s' % abs_initial_path)
        else:
            self.abs_initial_path = abs_initial_path
            self.walker()

    def __del__(self):
        """destructor"""
        os.chdir(self.abs_return_path)

    def is_path_to_ignore(self, path):
        path_parts = path.split(os.sep)
        check_list = map(lambda folder_part_to_ignore: folder_part_to_ignore in path_parts,
                         self.ignore_if_folder_parts_include_set)
        return any(check_list)

    def walker(self):
        """
        Recursively visit folders under root
        Process folder if not to be ignored
        
        Returns
        -------

        """
        for path, dirs, files in os.walk(self.abs_initial_path):
            # if not in ignore
            key = path
            self.result[key] = []
            if not self.is_path_to_ignore(path):
                folder_list = self.process_folder(path, files)
                self.result[key].append(('folder_list', folder_list))
                if folder_list:
                    if self.b_rel_path_key:
                        key = os.path.relpath(path, self.abs_initial_path)
                    else:
                        key = path
                    self.result[key] = folder_list
                else:
                    self.result[key].append(('folder_list', folder_list))
            else:
                self.result[key].append('ignore : %s' % path)

    def process_folder(self, folder, files):
        """
        
        Parameters
        ----------
        folder path string
        files an iterable containing filenames in folder

        Returns
        -------
        {file_name: [process_file() result], ...}
        """
        current_path = os.getcwd()
        os.chdir(os.path.abspath(folder))

        folder_result = {}

        for file_name in files:
            file_result = self.process_file(folder, file_name)
            if file_result:
                folder_result[file_name] = file_result

        os.chdir(current_path)

        return folder_result

    def process_file(self, path, file_name):
        """
        
        Parameters
        ----------
        path
        file_name

        Returns
        -------
        [(line number, text line including self.pattern), ...] 
        """

        # if file extension correct
        if os.path.splitext(file_name)[-1] != self.extension:
            result = []
        else:
            # read file
            with open(file_name, 'r', encoding='utf8') as f:
                lines = f.readlines()

            # collect line number and line text including the pattern
            result = [(line_number, line_text) for line_number, line_text in enumerate(lines) if self.pattern in line_text]

        return result


class RecursiveFinderFortran(RecursiveFinder):
    def __init__(self, initial_path=get_first_script_parameter(), extension='.f', pattern="CALL", function_list=None):

        if function_list is None:
            self.b_gotta_catch_em_all = True
        else:
            self.b_gotta_catch_em_all = False
            self.function_list = function_list

        super(RecursiveFinderFortran, self).__init__(initial_path=initial_path, extension=extension, pattern=pattern)

    def process_file(self, path, file_name):
        """
        Process the file if in the function list
        Parameters
        ----------
        path
        file_name

        Returns
        -------

        """
        result = []

        function_name = os.path.splitext(file_name)[0].lower()

        # if self.function_list is empty or function_name is in self.function_list
        if self.b_gotta_catch_em_all or (function_name in self.function_list):
            result = RecursiveFinder.process_file(self, path=path, file_name=file_name)

        return result


class FindFunctionNamesFromImport(object):
    def __init__(self, finder_result_dict):
        self.finder_result_dict = finder_result_dict
        self.function_names = {}

    def find_function_names(self):
        for path, files in self.finder_result_dict.items():
            if isinstance(files, dict):
                for filename, lines in files.items():
                    for line_number, line in lines:
                        self.handle_line_if_not_comment(line, path, filename, line_number)
            else:
                self.function_names[path] = files

        return self.function_names

    # TODO : More Testable Code : line -> function name
    @staticmethod
    def is_comment(line):
        """
        python version
        Parameters
        ----------
        line

        Returns
        -------

        """
        return '#' == line.strip()[0]

    def handle_line_if_not_comment(self, line, path, filename, line_number):
        if not self.is_comment(line):
            self.handle_file(filename, line, line_number, path)

    def handle_file(self, filename, line, line_number, path):
        """
        find function names from import line
        Parameters
        ----------
        filename
        line
        line_number
        path

        Returns
        -------

        """
        line_strip = line.strip()
        line_strip_split = line_strip.split()
        self.handle_two_functions_import(line_strip_split, path, filename, line_number)

    def handle_two_functions_import(self, line_strip_split, path, filename, line_number):
        function_name_list = self.find_function_names_from_import(line_strip_split)
        for key in function_name_list:
            self.add_function_name(key, path, filename, line_number)

    @staticmethod
    def find_function_names_from_import(line_strip_split):
        function_name_list = [function_name.strip(',') for function_name in line_strip_split[3:]]
        return function_name_list

    def add_function_name(self, key, path, filename, line_number):
        if key in self.function_names:
            self.function_names[key].append((path, filename, line_number))
        else:
            self.function_names[key] = [(path, filename, line_number)]


class FindFunctionUsedFortran(FindFunctionNamesFromImport):
    def __init__(self, finder_result_dict, function_names=None):
        FindFunctionNamesFromImport.__init__(self, finder_result_dict)
        self.finder_result_dict = finder_result_dict
        if function_names is None:
            self.function_names = {}
        elif isinstance(function_names, dict):
            self.function_names = copy.deepcopy(function_names)
        else:
            raise TypeError('function_names expected to be a dictionary')

    @staticmethod
    def is_comment(fortran_line):
        """
        If a line's first column is C, it is a comment
        Parameters
        ----------
        fortran_line

        Returns
        -------

        """
        result = fortran_line[0].strip()
        return result

    def handle_file(self, filename, line, line_number, path):
        """
        find function name from call line
        and add Fortran file path with filename and line number
        Parameters
        ----------
        filename
        line
        line_number
        path

        Returns
        -------

        """
        function_name = self.find_function_name_from_call_line(line)
        self.add_function_name(function_name, os.path.join('slycot', 'src'), filename, line_number)

    @staticmethod
    def find_function_name_from_call_line(line):
        """
        find string between 'CALL' and '('
        Parameters
        ----------
        line

        Returns
        -------

        """
        # string between CALL and '('
        function_name_candidate = re.findall(r'CALL\s*(.*?)\s*\(', line)
        function_name = function_name_candidate[0].lower()
        return function_name


def main(python_control_path, slycot_path):
    # from python import lines, find fortran function names
    python_finder = RecursiveFinder(python_control_path)

    slycot_fortran_file_name_set = set(
        [os.path.splitext(filename)[0].lower() for filename in os.listdir(os.path.join(slycot_path, 'slycot', 'src')) if
         '.f' == os.path.splitext(filename)[-1]])

    python_function_finder = FindFunctionNamesFromImport(python_finder.result)
    function_names = python_function_finder.find_function_names()

    function_name_set = set(function_names.keys())

    print("imported ".ljust(60, '#'))
    pprint(function_names)
    print("end imported ".ljust(60, '*'))

    # from fortran CALL lines, find selected fortran function names
    # find lines calling subroutines from Fortran source codes recursively visiting sub-folders

    fortran_finder = RecursiveFinderFortran(slycot_path, function_list=function_name_set, pattern='CALL')

    fortran_function_finder = FindFunctionUsedFortran(fortran_finder.result, function_names)
    fortran_call_dict = fortran_function_finder.find_function_names()
    print("called ".ljust(60, '#'))
    print_md_table(fortran_call_dict, function_name_set, slycot_fortran_file_name_set)
    print("end called ".ljust(60, '*'))

    # find go to lines from Fortran source codes recursively visiting sub-folders
    fortran_go_to_set = find_in_tree(slycot_path, function_name_set, 'GO')

    # find goto lines from Fortran source codes recursively visiting sub-folders
    fortran_go_to_set = fortran_go_to_set.union(find_in_tree(slycot_path, function_name_set, 'GOTO'))

    print(("# fortran files with go to or goto = %d " % len(fortran_go_to_set)).ljust(60, '#'))
    for function_name in sorted(list(fortran_go_to_set)):
        print(function_name.lower()[:-2])
    print("end go to or goto ".ljust(60, '*'))


def print_md_table(call_info_dict, slycot_pyctrl_set, slycot_fortran_file_name_set):
    print("| function names (%d) | library | # calls | call locations |" % len(call_info_dict))
    print("|:----------------:|:----------:|:------:|:--------------:|")

    """
   scipy.linalg.blas – Low-level BLAS functions
   scipy.linalg.lapack – Low-level LAPACK functions
   scipy.linalg.cython_blas – Low-level BLAS functions for Cython
   scipy.linalg.cython_lapack – Low-level LAPACK functions for Cython
   """

    blas_set = set(dir(blas))
    lapack_set = set(dir(lapack))
    cython_blas_set = set(cython_blas.__pyx_capi__.keys())
    cython_lapack_set = set(cython_lapack.__pyx_capi__.keys())
    np_lapack_lite_set = set(dir(np_lapack_lite))

    function_name_list = list(call_info_dict.keys())
    function_name_list.sort()
    function_name_list.sort(key=lambda x: len(call_info_dict[x]), reverse=True)

    for function_name in function_name_list:

        lib_name = ''
        if function_name in slycot_pyctrl_set:
            lib_name = 'slycot(d)'
        elif function_name in slycot_fortran_file_name_set:
            lib_name = 'slycot(i)'
        elif function_name in np_lapack_lite_set:
            lib_name = 'numpy.linalg.lapack_lite'
        elif function_name in cython_lapack_set:
            lib_name = 'scipy.linalg.cython_lapack'
        elif function_name in cython_blas_set:
            lib_name = 'scipy.linalg.cython_blas'
        elif function_name in lapack_set:
            lib_name = 'scipy.linalg.lapack'
        elif function_name in blas_set:
            lib_name = 'scipy.linalg.blas'

        call_record_list = [str(call_record_set) for call_record_set in call_info_dict[function_name]]

        print('| %s | %s | %d | %s |' % (function_name, lib_name, len(call_info_dict[function_name]),
                                         '<br>'.join(call_record_list)))


def find_in_tree(slycot_path, function_tuple, pattern_string):
    fortran_go_to = RecursiveFinderFortran(slycot_path, function_list=function_tuple, pattern=pattern_string)
    print(("with %r " % pattern_string).ljust(60, '#'))

    fortran_go_to_set = set()
    if fortran_go_to.result.values():
        pprint(fortran_go_to.result)
        for fortran_path in fortran_go_to.result:
            search_result = fortran_go_to.result[fortran_path]
            fortran_go_to_set = fortran_go_to_set.union(search_result.keys())
    else:
        print('%r not found' % pattern_string)

    print(("end with %r to " % pattern_string).ljust(60, '*'))
    return fortran_go_to_set


if __name__ == '__main__':
    import sys

    try:
        main(sys.argv[1], sys.argv[2])
    except IndexError as e:
        message = 'Find slycot functions directly or indirectly called in python-control\n' \
                  'Example: $ python find_slycot.py [path to python-control] [path to slycot]'
        print(message)
