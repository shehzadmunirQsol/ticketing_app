import React, { useState } from 'react';
import {
  ColumnDef,
  // ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown, MoreHorizontal } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import LanguageSelect, { LanguageInterface } from '../language_select';
import { trpc } from '~/utils/trpc';
import Image from 'next/image';
import { displayDate, renderNFTImage } from '~/utils/helper';
import Link from 'next/link';
import { GetEventSchema } from '~/schema/event';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '../modal/loadingModal';
import { setSelectedEvent } from '~/store/reducers/admin_layout';
import { useDispatch } from 'react-redux';
import { TableFilters } from './table_filters';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons';
import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';

export type Category = {
  thumb: string;
  name: string;
  desc: string | null;
  id: number;
  price: number;
  total_tickets: number;
  tickets_sold: number;
  user_ticket_limit: number;
  launch_date: Date;
  end_date: Date;
  created_at: Date;
  updated_at: Date;
};

export default function EventsDataTable() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filterID, setFilterID] = useState({});

  const [filters, setFilters] = useState<GetEventSchema>({
    first: 0,
    rows: 10,
    lang_id: 1,
  });
  console.log({ filterID });
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});

  const dispatch = useDispatch();

  const { data, isLoading } = trpc.event.get.useQuery(
    { ...filters, filters: { ...filterID } },
    {
      refetchOnWindowFocus: false,
    },
  );
  const { data: categoryData } = trpc.category.getCategory.useQuery({
    lang_id: 1,
  });

  const eventData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);
  const columns: ColumnDef<Category>[] = [
    {
      accessorKey: 'Name',
      header: 'Name',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <Image
              className="object-cover bg-ac-2 h-10 w-16 rounded-lg"
              src={renderNFTImage(row.original)}
              alt={row?.original?.name}
              width={100}
              height={100}
            />

            <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
              {row?.original?.name}
            </p>
          </div>
        );
      },
    },
    {
      accessorKey: 'Description',
      header: 'Description',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap overflow-hidden w-64">
          {row?.original?.desc}
        </div>
      ),
    },
    {
      accessorKey: 'Token Price',
      header: 'Token Price',
      cell: ({ row }) => (
        <p className="w-20 text-center text-ellipsis whitespace-nowrap overflow-hidden">
          {(row?.original?.price).toFixed(2)}
        </p>
      ),
    },
    {
      accessorKey: 'Token Cap',
      header: 'Token Cap',
      cell: ({ row }) => (
        <p className="w-20 text-ellipsis whitespace-nowrap overflow-hidden">
          {row?.original?.total_tickets}
          &nbsp;
          <sub>qty</sub>
        </p>
      ),
    },
    {
      accessorKey: 'Token Purchased',
      header: 'Token Purchased',
      cell: ({ row }) => (
        <p className="w-28 text-center text-ellipsis whitespace-nowrap overflow-hidden">
          {row?.original?.tickets_sold}
          &nbsp;
          <sub>qty</sub>
        </p>
      ),
    },
    {
      accessorKey: 'Per User Limit',
      header: 'Per User Limit',
      cell: ({ row }) => (
        <p className="w-32 text-center text-ellipsis whitespace-nowrap overflow-hidden">
          {row?.original?.user_ticket_limit}
          &nbsp;
          <sub>qty</sub>
        </p>
      ),
    },
    {
      accessorKey: 'Launch Date',
      header: 'Launch Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
          {displayDate(row?.original?.launch_date)}
        </div>
      ),
    },
    {
      accessorKey: 'End Date',
      header: 'End Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
          {displayDate(row?.original?.end_date)}
        </div>
      ),
    },
    {
      id: 'actions',
      enableHiding: false,
      header: 'Actions',
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuLabel>Actions</DropdownMenuLabel>
              <DropdownMenuSeparator />

              <Link href={`/admin/events/edit/${row?.original?.id}`}>
                <DropdownMenuItem>Edit Event</DropdownMenuItem>
              </Link>

              {row?.original?.tickets_sold > 0 ? (
                <>
                  <DropdownMenuSeparator />
                  <Link
                    onClick={() => dispatch(setSelectedEvent(row.original))}
                    href={`/admin/events/event-customers/${row.original.id}`}
                  >
                    <DropdownMenuItem>Event Customers</DropdownMenuItem>
                  </Link>
                </>
              ) : null}
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];
  const table = useReactTable({
    data: eventData as Category[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function languageHandler(params: LanguageInterface) {
    setFilters((prevFilters) => ({ ...prevFilters, lang_id: params.id }));
  }

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }

  // FILTER OPTIONS
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Event Status',
      filtername: 'status',
      type: 'select',

      filter: [
        {
          name: 'Active',
          value: 'active',
        },
        {
          name: 'Closed',
          value: 'in-active',
        },
      ],
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Category',
      filtername: 'category_id',
      type: 'select',

      filter: categoryData,
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'Launch Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'End Date',
      filtername: 'endDate',
      type: 'date',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-end gap-2">
        <LanguageSelect languageHandler={languageHandler} />
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
        <TableFilters
          inputList={roleOptions1}
          item_name={'Events'}
          value={filterID}
          setValue={setFilterID}
          setFilters={setFilters}
          initial={filters}
        />
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader>
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
          Page {filters.first + 1} of{' '}
          {Math.ceil((data?.count ?? 0) / filters.rows)}
        </div>

        <div className="flex items-center justify-center space-x-6 lg:space-x-8">
          <div className="flex items-center space-x-2">
            <p className="text-sm font-medium">Rows per page</p>
            <Select
              value={`${filters.rows}`}
              onValueChange={(value) => {
                setFilters((prevFilters: any) => ({
                  ...prevFilters,
                  rows: Number(value),
                  first: 0,
                }));
                table.setPageSize(Number(value));
              }}
            >
              <SelectTrigger className="h-8 w-[70px]">
                <SelectValue placeholder={filters.rows} />
              </SelectTrigger>
              <SelectContent side="top">
                {[5, 10, 20, 30, 40, 50].map((pageSize) => (
                  <SelectItem key={pageSize} value={`${pageSize}`}>
                    {pageSize}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="flex items-center space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => handlePagination(0)}
              disabled={filters.first === 0}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters?.first - 1)}
              disabled={filters?.first === 0}
            >
              <span className="sr-only">Go to previous page</span>
              <ChevronLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters.first + 1)}
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to next page</span>
              <ChevronRightIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() =>
                handlePagination(
                  Math.ceil((data?.count ?? 0) / filters.rows) - 1,
                )
              }
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>

      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
